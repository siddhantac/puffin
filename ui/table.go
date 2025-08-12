package ui

import (
	"fmt"
	"log"
	"strings"

	"github.com/charmbracelet/bubbles/table"
	"github.com/charmbracelet/bubbles/textinput"
	tea "github.com/charmbracelet/bubbletea"
	"github.com/charmbracelet/lipgloss"
	"github.com/siddhantac/hledger"
	"github.com/siddhantac/puffin/ui/colorscheme"
)

type TableData interface {
	Columns() table.Row
	Rows() []table.Row
}

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

	t.SetColumns(data[0])

	rows := data[1:]

	for _, dt := range t.dataTransformers {
		if err := dt.Transform(rows); err != nil {
			t.log(fmt.Sprintf("data transform error: %v", err))
		}
	}
	t.SetRows(rows)
	t.numRows = len(rows)

	t.isDataReady = true
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
		t.Model.SetHeight(t.size.Height)
		
		// Update filter width for register tables
		if t.cmdType == cmdRegister {
			t.registerFilter.Width = t.size.Width - 4 // leave some padding
		}

	case tea.KeyMsg:
		// Handle register filter specific keys
		if t.cmdType == cmdRegister {
			switch msg.String() {
			case "ctrl+f", "/":
				// Focus the filter input
				t.filterFocused = true
				return t, t.registerFilter.Focus()
			case "esc":
				if t.filterFocused {
					// Unfocus the filter input
					t.filterFocused = false
					t.registerFilter.Blur()
					return t, nil
				}
			case "enter":
				if t.filterFocused {
					// Apply filter and unfocus
					t.filterFocused = false
					t.registerFilter.Blur()
					// Trigger a refresh with the new filter
					return t, func() tea.Msg {
						return filterApplied{}
					}
				}
			}
			
			// If filter is focused, let it handle the input
			if t.filterFocused {
				t.registerFilter, cmd = t.registerFilter.Update(msg)
				return t, cmd
			}
		}
		
		switch msg.String() {
		// case key.Matches(msg, allKeys.ScrollUp):
		case "K":
			t.Model.MoveUp(1)
			// case key.Matches(msg, allKeys.ScrollDown):
		case "J":
			t.Model.MoveDown(1)
		}
	default:
		_, cmd = t.Model.Update(msg)
	}

	return t, cmd
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

	// Create filter input box
	filterLabelStyle := lipgloss.NewStyle().
		Bold(true).
		Foreground(lipgloss.Color(colorscheme.GruvboxSkyBlue)).
		MarginRight(1)
	filterBoxStyle := lipgloss.NewStyle().
		BorderStyle(lipgloss.RoundedBorder()).
		BorderForeground(lipgloss.Color("240")).
		Padding(0, 1).
		MarginBottom(1)
		
	filterLabel := filterLabelStyle.Render("Filter:")
	filterInput := t.registerFilter.View()
	filterRow := lipgloss.JoinHorizontal(lipgloss.Center, filterLabel, filterInput)
	filterBox := filterBoxStyle.Render(filterRow)
	
	// Add help text
	helpStyle := lipgloss.NewStyle().
		Foreground(lipgloss.Color("240")).
		Italic(true)
	helpText := ""
	if !t.filterFocused {
		helpText = helpStyle.Render("Press Ctrl+F or / to filter, current: " + t.registerFilter.Value())
	} else {
		helpText = helpStyle.Render("Enter to apply filter, Esc to cancel")
	}

	// Create styles for alternating rows
	lightGreenStyle := lipgloss.NewStyle().
		Background(lipgloss.Color(colorscheme.GruvboxLightGreen)).
		Padding(0, 1)
	lighterGreenStyle := lipgloss.NewStyle().
		Background(lipgloss.Color(colorscheme.GruvboxLighterGreen)).
		Padding(0, 1)
	selectedStyle := lipgloss.NewStyle().
		Background(lipgloss.Color(colorscheme.GruvboxSkyBlue)).
		Foreground(lipgloss.Color("#ffffff")).
		Padding(0, 1)
	headerStyle := lipgloss.NewStyle().
		BorderStyle(lipgloss.NormalBorder()).
		BorderForeground(lipgloss.Color("240")).
		BorderBottom(true).
		Bold(true).
		Padding(0, 1)

	// Build header row
	headerRow := ""
	for i, col := range t.columns {
		cellContent := headerStyle.Width(col.Width).Render(col.Title)
		if i == 0 {
			headerRow = cellContent
		} else {
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
				cellContent := style.Width(t.columns[j].Width).Render(cell)
				if j == 0 {
					rowStr = cellContent
				} else {
					rowStr = lipgloss.JoinHorizontal(lipgloss.Top, rowStr, cellContent)
				}
			}
		}
		rows = append(rows, rowStr)
	}

	// Join all rows vertically
	tableContent := lipgloss.JoinVertical(lipgloss.Left, rows...)

	// Add border around the table
	borderStyle := lipgloss.NewStyle().
		BorderStyle(lipgloss.NormalBorder()).
		BorderForeground(lipgloss.Color("240"))
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

	if cmdType == cmdRegister {
		tbl.SetStyles(getRegisterTableStyle())
	} else {
		tbl.SetStyles(getTableStyle())
	}
	return &tbl
}

func (t *Table) SetColumns(firstRow table.Row) {
	// if len(t.columnPercentages) == 0 {
	t.columnPercentages = make([]int, 0, len(firstRow))
	for range firstRow {
		t.columnPercentages = append(t.columnPercentages, 100/len(firstRow))
	}
	// }
	// if len(t.columnPercentages) != len(firstRow) {
	// 	panic(fmt.Sprintf("length not equal: expected=%d, got=%d", len(t.columnPercentages), len(firstRow)))
	// }

	cols := make([]table.Column, 0, len(firstRow))
	for i, row := range firstRow {
		c := table.Column{Title: row, Width: percent(t.size.Width, t.columnPercentages[i])}
		cols = append(cols, c)
	}

	if len(cols) != len(t.columns) {
		t.columns = cols
		t.Model = newDefaultTable(cols, t.cmdType)
		t.Model.SetHeight(t.size.Height)
		t.Model.SetWidth(t.size.Width)
	}
}
