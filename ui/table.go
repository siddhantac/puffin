package ui

import (
	"fmt"
	"log"
	"strings"
	"time"

	"github.com/charmbracelet/bubbles/table"
	"github.com/charmbracelet/bubbles/textinput"
	tea "github.com/charmbracelet/bubbletea"
	"github.com/charmbracelet/lipgloss"
	"github.com/siddhantac/hledger"
)

type Table struct {
	*table.Model
	name              string
	size              size
	columnPercentages []int
	columns           []table.Column
	isDataReady       bool
	id                int
	cmd               func(int, hledger.Options) content
	locked            bool
	cmdType           cmdType
	dataTransformers  []dataTransformer
	numRows           int
	registerFilter    textinput.Model
	filterFocused     bool
}

func newTable(name string, columnPercentages []int, id int, cmd func(int, hledger.Options) content, locked bool, cmdType cmdType, dataTransformers []dataTransformer) *Table {
	t := &Table{
		id:                id,
		cmd:               cmd,
		locked:            locked,
		name:              name,
		cmdType:           cmdType,
		columnPercentages: columnPercentages,
		Model:             &table.Model{},
		dataTransformers:  dataTransformers,
	}
	
	// Initialize register filter for register tables
	if cmdType == cmdRegister {
		t.registerFilter = textinput.New()
		t.registerFilter.Placeholder = "Filter register (e.g., rent, chase, groceries)..."
		t.registerFilter.CharLimit = 50
		t.registerFilter.Width = 50
	}
	
	return t
}

func (t *Table) log(msg string) {
	log.Printf("%s(%d): %s", t.name, t.id, msg)
}

func (t *Table) Type() cmdType { return t.cmdType }
func (t *Table) Locked() bool  { return t.locked }
func (t *Table) IsReady() bool { return t.isDataReady }
func (t *Table) SetUnready() {
	t.isDataReady = false
	t.log("unready")
}
func (t *Table) NumRows() int { return t.numRows }

// GetRegisterFilter returns the register filter value
func (t *Table) GetRegisterFilter() string {
	if t.cmdType == cmdRegister {
		return t.registerFilter.Value()
	}
	return ""
}

// IsFilterFocused returns whether the register filter is currently focused
func (t *Table) IsFilterFocused() bool {
	if t.cmdType == cmdRegister {
		return t.filterFocused
	}
	return false
}

func (t *Table) Run(options hledger.Options) tea.Cmd {
	return func() tea.Msg {
		return t.cmd(t.id, options)
	}
}

func (t *Table) SetContent(gc content) {
	if gc.id != t.id {
		return
	}

	t.isDataReady = false
	data, err := parseCSV(strings.NewReader(gc.msg))
	if err != nil {
		t.log(fmt.Sprintf("csv parse error: %v", err))
		return
	}

	// For Revenue balance report, append Q1, Q2, YTD columns (current year) by aggregating monthly values
	if t.cmdType == cmdBalance && strings.EqualFold(strings.TrimSpace(t.name), "revenue") {
		data = addRevenueQuarterYTDColumns(data)
	}

	// Special handling for Balance Sheet: drop Commodity column and rename current month column to today's date
	if t.cmdType == cmdBalanceSheet {
		data = transformBalanceSheetData(data)
	}

	t.SetColumns(data[0])

	rows := data[1:]

	for _, dt := range t.dataTransformers {
		if err := dt.Transform(rows); err != nil {
			t.log(fmt.Sprintf("data transform error: %v", err))
		}
	}

	// Normalize each row to match the configured number of columns to avoid
	// bubbles/table panics when rows have more cells than headers (or fewer).
	norm := make([]table.Row, len(rows))
	for i, r := range rows {
		out := make(table.Row, len(t.columns))
		for j := 0; j < len(t.columns); j++ {
			if j < len(r) {
				out[j] = r[j]
			} else {
				out[j] = ""
			}
		}
		norm[i] = out
	}
	
	t.SetRows(norm)
	t.numRows = len(norm)

	t.isDataReady = true
}

// transformBalanceSheetData removes the Commodity column and updates the last date header to today's date.
// Expects data as [][]string where data[0] is the header row.
func transformBalanceSheetData(data []table.Row) []table.Row {
	if len(data) == 0 || len(data[0]) < 3 {
		return data
	}
	// Remove the second column (Commodity) from header and all rows if present
	header := data[0]
	if len(header) >= 2 {
		data[0] = append([]string{header[0]}, header[2:]...)
	}
	// Update the last header (current month) to today's date (YYYY-MM-DD)
	if len(data[0]) >= 2 {
		lastIdx := len(data[0]) - 1
		// Attempt to replace a date-like header with today's date string
		nowStr := time.Now().Format("2006-01-02")
		data[0][lastIdx] = nowStr
	}
	// Process each data row: drop commodity column
	for i := 1; i < len(data); i++ {
		row := data[i]
		if len(row) >= 2 {
			data[i] = append([]string{row[0]}, row[2:]...)
		}
	}
	return data
}

func (t *Table) Init() tea.Cmd {
	t.SetUnready()
	return nil
}

func (t *Table) Update(msg tea.Msg) (tea.Model, tea.Cmd) {
	var cmd tea.Cmd
	switch msg := msg.(type) {
	case tea.WindowSizeMsg:
			t.size = size{
				Width:  percent(msg.Width, 99),
				Height: msg.Height - 4, // 3 for header row, 1 for the border at the bottom
			}
			t.log(fmt.Sprintf("tableSize: %s", t.size))

			t.Model.SetWidth(t.size.Width)
			heightToSet := t.size.Height
			if t.cmdType == cmdRegister {
				if heightToSet > 53 { // header + max 52 entries
					heightToSet = 53
				}
			}
			t.Model.SetHeight(heightToSet)

			// Recompute column widths based on the new size to avoid zero-width columns
			t.recomputeColumnWidths()
			
			// Update filter width for register tables
			if t.cmdType == cmdRegister {
				t.registerFilter.Width = t.size.Width - 4 // leave some padding
			}

	case tea.KeyMsg:
		// Handle register filter keys - highest priority when filter is focused
		if t.cmdType == cmdRegister && t.filterFocused {
			// When filter is focused, handle escape and enter specially, otherwise let textinput handle everything
			switch msg.String() {
			case "esc":
				// Unfocus the filter input
				t.filterFocused = false
				t.registerFilter.Blur()
				return t, nil
			case "enter":
				// Apply filter and unfocus
				t.filterFocused = false
				t.registerFilter.Blur()
				// Trigger a refresh with the new filter
				return t, func() tea.Msg {
					return filterApplied{}
				}
			default:
				// Let the text input handle all other keys when focused
				t.registerFilter, cmd = t.registerFilter.Update(msg)
				return t, cmd
			}
		}
		
		// Handle register filter activation keys (when not focused)
		if t.cmdType == cmdRegister {
			switch msg.String() {
			case "ctrl+f", "/":
				if !t.filterFocused {
					// Focus the filter input
					t.filterFocused = true
					return t, t.registerFilter.Focus()
				}
			}
		}
		
		// Handle page-wise scrolling and forward other keys to the underlying table model
		if !t.filterFocused {
			switch msg.String() {
			case "pgup", "pageup":
				// Page Up: move cursor up by one page
				page := t.pageSize()
				cur := t.Model.Cursor()
				newCur := cur - page
				if newCur < 0 { newCur = 0 }
				t.Model.SetCursor(newCur)
				return t, nil
			case "pgdown", "pagedown":
				// Page Down: move cursor down by one page
				page := t.pageSize()
				cur := t.Model.Cursor()
				rows := len(t.Model.Rows())
				if rows > 0 {
					newCur := cur + page
					if newCur >= rows { newCur = rows - 1 }
					if newCur < 0 { newCur = 0 }
					t.Model.SetCursor(newCur)
				}
				return t, nil
			case "home":
				// Jump to first row
				t.Model.SetCursor(0)
				return t, nil
			case "end":
				// Jump to last row
				rows := len(t.Model.Rows())
				if rows > 0 { t.Model.SetCursor(rows - 1) }
				return t, nil
			}
			// Forward all other key presses (arrows, etc.) to the table model
			_, cmd = t.Model.Update(msg)
			return t, cmd
		}
	default:
		_, cmd = t.Model.Update(msg)
	}

	return t, cmd
}

// pageSize returns the number of rows to move for a page-wise scroll, matching
// the visible area used in renderRegisterTable (1 row reserved for header, max 52).
func (t *Table) pageSize() int {
	visible := t.size.Height - 1
	if visible <= 0 {
		visible = 20 // sensible default when size isn't set yet
	}
	if visible > 52 {
		visible = 52
	}
	return visible
}

func (t *Table) View() string {
	if t.cmdType == cmdRegister {
		return t.renderRegisterTable()
	}
	s := lipgloss.NewStyle().
		BorderStyle(lipgloss.NormalBorder()).
		BorderForeground(lipgloss.Color("240")).
		BorderBottom(true)
	return s.Render(t.Model.View())
}

func (t *Table) renderRegisterTable() string {
	if !t.isDataReady || t.Model == nil {
		return ""
	}

	// Create filter input box with theme styling
	filterLabelStyle := lipgloss.NewStyle().
		Bold(true).
		Foreground(theme.SecondaryColor). // Use theme color
		MarginRight(1)
	filterBoxStyle := lipgloss.NewStyle().
		BorderStyle(lipgloss.RoundedBorder()).
		BorderForeground(theme.PrimaryForeground). // Use theme color
		Background(theme.PrimaryBackground). // Use theme background
		Padding(0, 1).
		MarginBottom(1)
		
	filterLabel := filterLabelStyle.Render("Filter:")
	filterInput := t.registerFilter.View()
	filterRow := lipgloss.JoinHorizontal(lipgloss.Center, filterLabel, filterInput)
	filterBox := filterBoxStyle.Render(filterRow)
	
	// Add help text using theme colors
	helpStyle := lipgloss.NewStyle().
		Foreground(theme.SecondaryColor). // Use theme color
		Italic(true)
	helpText := ""
	if !t.filterFocused {
		helpText = helpStyle.Render("Press Ctrl+F or / to filter, current: " + t.registerFilter.Value())
	} else {
		helpText = helpStyle.Render("Enter to apply filter, Esc to cancel")
	}

	// Create styles for alternating rows using theme colors
	lightGreenStyle := lipgloss.NewStyle().
		Background(theme.SecondaryBackground).
		Foreground(theme.PrimaryColor).
		Padding(0, 1)
	lighterGreenStyle := lipgloss.NewStyle().
		Background(theme.PrimaryBackground).
		Foreground(theme.PrimaryColor).
		Padding(0, 1)
	selectedStyle := lipgloss.NewStyle().
		Background(theme.Accent).
		Foreground(theme.PrimaryForeground).
		Padding(0, 1)
	headerStyle := lipgloss.NewStyle().
		BorderStyle(lipgloss.NormalBorder()).
		BorderForeground(theme.SecondaryForeground).
		BorderBottom(true).
		Bold(true).
		Foreground(theme.PrimaryColor).
		Background(theme.SecondaryBackground).
		Padding(0, 1)

	// Build header row
	headerRow := ""
	for i, col := range t.columns {
		// Apply minimal padding to txnidx header (first column)
		if i == 0 {
			// Use PaddingLeft(0) for the txnidx column header to minimize left margin
			txnidxHeaderStyle := headerStyle.Copy().PaddingLeft(0).Width(col.Width)
			cellContent := txnidxHeaderStyle.Render(col.Title)
			headerRow = cellContent
		} else {
			cellContent := headerStyle.Width(col.Width).Render(col.Title)
			headerRow = lipgloss.JoinHorizontal(lipgloss.Top, headerRow, cellContent)
		}
	}

	// Get visible rows based on table height and current cursor position
	allRows := t.Model.Rows()
	cursor := t.Model.Cursor()
	tableHeight := t.size.Height
	if tableHeight <= 0 {
		tableHeight = 20 // default height
	}

		// Calculate visible range
		visibleRows := tableHeight - 1 // subtract 1 for header
		if visibleRows > 52 {
			visibleRows = 52
		}
		start := 0
		end := len(allRows)

	if len(allRows) > visibleRows {
		// Calculate scroll offset to keep cursor in view
		start = cursor - visibleRows/2
		if start < 0 {
			start = 0
		}
		end = start + visibleRows
		if end > len(allRows) {
			end = len(allRows)
			start = end - visibleRows
			if start < 0 {
				start = 0
			}
		}
	}

	// Build data rows with alternating colors for visible range
	rows := []string{headerRow}
	for i := start; i < end; i++ {
		if i >= len(allRows) {
			break
		}
		row := allRows[i]
		rowStr := ""
		var style lipgloss.Style
		
		// Determine style based on selection and alternating pattern
		if i == cursor {
			style = selectedStyle
		} else if i%2 == 0 {
			style = lightGreenStyle
		} else {
			style = lighterGreenStyle
		}

		for j, cell := range row {
			if j < len(t.columns) {
				// Apply minimal left padding for the txnidx column (first column)
				if j == 0 {
					// Use PaddingLeft(0) for the txnidx column to minimize left margin
					txnidxStyle := style.Copy().PaddingLeft(0).Width(t.columns[j].Width)
					cellContent := txnidxStyle.Render(cell)
					rowStr = cellContent
				} else {
					cellContent := style.Width(t.columns[j].Width).Render(cell)
					rowStr = lipgloss.JoinHorizontal(lipgloss.Top, rowStr, cellContent)
				}
			}
		}
		rows = append(rows, rowStr)
	}

	// Join all rows vertically
	tableContent := lipgloss.JoinVertical(lipgloss.Left, rows...)

	// Add border around the table using theme colors
	borderStyle := lipgloss.NewStyle().
		BorderStyle(lipgloss.NormalBorder()).
		BorderForeground(theme.SecondaryForeground) // Use theme color
	borderedTable := borderStyle.Render(tableContent)

	// Combine filter box, help text, and table
	finalContent := lipgloss.JoinVertical(
		lipgloss.Left,
		filterBox,
		helpText,
		borderedTable,
	)

	return finalContent
}

func percent(number, percentage int) int {
	return (percentage * number) / 100
}

func newDefaultTable(columns []table.Column, cmdType cmdType) *table.Model {
	tbl := table.New(
		table.WithColumns(columns),
		table.WithKeyMap(table.DefaultKeyMap()),
		table.WithFocused(true),
		// table.WithHeight(22),
	)

switch cmdType {
	case cmdRegister:
		tbl.SetStyles(getRegisterTableStyle())
	case cmdBalanceSheet:
		tbl.SetStyles(getBalanceSheetTableStyle())
	default:
		tbl.SetStyles(getTableStyle())
	}
	return &tbl
}

func (t *Table) SetColumns(firstRow table.Row) {
	// Determine previous column count before we compute new columns
	prevColsLen := len(t.columns)

	// Set custom column percentages for register tables
	if t.cmdType == cmdRegister && len(firstRow) == 7 {
		// Custom widths for register with 7 columns: txnidx(7 chars), date(13 chars), code(7 chars), then equal width for last 4
		// Using fixed character widths: txnidx=7, date=13, code=7, remaining split equally among description, account, amount, total
		// Approximate percentages based on typical terminal width of ~100 characters
		t.columnPercentages = []int{7, 13, 5, 19, 19, 19, 18} // txnidx, date, code, description, account, amount, total
	} else {
		// Default: equal width for all columns
		t.columnPercentages = make([]int, 0, len(firstRow))
		for range firstRow {
			t.columnPercentages = append(t.columnPercentages, 100/len(firstRow))
		}
	}

	cols := make([]table.Column, 0, len(firstRow))
	for i, row := range firstRow {
		var width int
		
		// For 7-column register tables, use fixed widths for first 3 columns, equal width for last 4
		if t.cmdType == cmdRegister && len(firstRow) == 7 {
			switch i {
			case 0: // txnidx
				width = 7
			case 1: // date
				width = 13
			case 2: // code
				width = 7
			default: // description, account, amount, total (last 4 columns)
				// Calculate remaining width after fixed columns, divide equally among last 4
				remainingWidth := t.size.Width - 27 // 27 = 7+13+7 fixed columns
				if remainingWidth < 40 { // ensure minimum usable width
					remainingWidth = 40
				}
				width = remainingWidth / 4
			}
		} else {
			// Use percentage-based width for other table types
			width = percent(t.size.Width, t.columnPercentages[i])
		}
		
		c := table.Column{Title: row, Width: width}
		cols = append(cols, c)
	}

	// Update model columns, recreating the model only if the structure (count) changed or model is uninitialized
	if t.Model == nil || len(cols) != prevColsLen {
		// Create a new model only if structure changed or model uninitialized
		t.Model = newDefaultTable(cols, t.cmdType)
	} else {
		// Update columns in-place to refresh widths
		t.Model.SetColumns(cols)
	}

	// Keep internal columns in sync with the model
	t.columns = cols

	// Ensure model size is kept in sync
	t.Model.SetHeight(t.size.Height)
		t.Model.SetWidth(t.size.Width)
}

// recomputeColumnWidths recalculates and applies column widths based on current size and existing column titles.
// It is safe to call when size changes or after initial data load.
func (t *Table) recomputeColumnWidths() {
	if t.Model == nil || len(t.columns) == 0 {
		return
	}
	// Build a firstRow from existing column titles
	firstRow := make(table.Row, len(t.columns))
	for i, c := range t.columns {
		firstRow[i] = c.Title
	}
	// Reuse SetColumns to compute widths and apply them
	t.SetColumns(firstRow)
}

// addRevenueQuarterYTDColumns appends Q1, Q2 and YTD columns to the Revenue balance table.
// It detects monthly date columns for the current year in the header and sums those per row.
func addRevenueQuarterYTDColumns(data []table.Row) []table.Row {
	if len(data) == 0 || len(data[0]) < 2 {
		return data
	}

	header := data[0]
	year := time.Now().Year()

	// Map header column index -> month number (1..12) for current year monthly columns
	monthCols := map[int]int{}
	for j := 1; j < len(header); j++ {
		title := strings.TrimSpace(header[j])
		if title == "" { continue }
		// Parse YYYY-MM-DD
		if ts, err := time.Parse("2006-01-02", title); err == nil {
			if ts.Year() == year {
				monthCols[j] = int(ts.Month())
			}
			continue
		}
		// Try YYYY-MM
		if len(title) >= 7 {
			if ts, err := time.Parse("2006-01", title[:7]); err == nil {
				if ts.Year() == year {
					monthCols[j] = int(ts.Month())
				}
			}
		}
	}
	if len(monthCols) == 0 {
		// Nothing to aggregate; return as-is
		return data
	}

	// Append new headers
	header = append(header, "Q1", "Q2", "YTD")

	out := make([]table.Row, 0, len(data))
	out = append(out, header)
	nowMonth := int(time.Now().Month())

	for i := 1; i < len(data); i++ {
		row := data[i]
		var q1, q2, ytd float64
		currency := ""
		for colIdx, mon := range monthCols {
			if colIdx >= len(row) { continue }
			cell := strings.TrimSpace(row[colIdx])
			if currency == "" && cell != "" {
				currency = detectCurrencyPrefix(cell)
			}
			v := parseAmount(cell)
			if mon >= 1 && mon <= 3 { q1 += v }
			if mon >= 4 && mon <= 6 { q2 += v }
			if mon >= 1 && mon <= nowMonth { ytd += v }
		}
		row = append(row, formatCurrency(currency, q1), formatCurrency(currency, q2), formatCurrency(currency, ytd))
		out = append(out, row)
	}
	return out
}

// detectCurrencyPrefix tries to extract a non-numeric prefix (e.g., "SGD$") from an amount string.
func detectCurrencyPrefix(s string) string {
	s = strings.TrimSpace(s)
	if s == "" { return "" }
	// Strip surrounding parentheses
	if strings.HasPrefix(s, "(") && strings.HasSuffix(s, ")") {
		s = strings.TrimSuffix(strings.TrimPrefix(s, "("), ")")
	}
	// Trim leading minus from the prefix area if present
	s = strings.TrimSpace(s)
	for i, r := range s {
		if (r >= '0' && r <= '9') || r == '.' {
			pref := strings.TrimSpace(s[:i])
			pref = strings.TrimSuffix(pref, "-")
			return pref
		}
	}
	return ""
}

func formatCurrency(prefix string, v float64) string {
	if prefix != "" {
		return fmt.Sprintf("%s%.2f", prefix, v)
	}
	return fmt.Sprintf("%.2f", v)
}
