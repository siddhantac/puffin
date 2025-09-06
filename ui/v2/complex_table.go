package ui

import (
	"github.com/siddhantac/puffin/ui/v2/interfaces"

	"github.com/charmbracelet/bubbles/table"
	tea "github.com/charmbracelet/bubbletea"
	"github.com/charmbracelet/lipgloss"
	"strings"
)

type complexTable struct {
	title, upperTitle, lowerTitle string
	bottomBar                     table.Model
	upper, lower                  table.Model
	focus                         bool
	columns                       []string
	// When true, render upper and lower side-by-side instead of stacked
	horizontalLayout              bool
	// Balance Sheet mode: purple headers and no row selection highlight
	isBalanceSheet                bool
}

func newComplexTable() *complexTable {
	return &complexTable{
		upper:     table.New(),
		lower:     table.New(),
		bottomBar: table.New(),
	}
}

func (c *complexTable) Focus() {
	c.focus = true
}

func (c *complexTable) Blur() {
	c.focus = false
}

func (c *complexTable) Focused() bool {
	return c.focus
}

func (c *complexTable) Init() tea.Cmd {
	c.upper.Focus()
	c.lower.Blur()
	return nil
}

func (c *complexTable) SetHorizontalLayout(h bool) {
	c.horizontalLayout = h
}

func (c *complexTable) Update(msg tea.Msg) (*complexTable, tea.Cmd) {
	if !c.focus {
		return c, nil
	}

	var cmd tea.Cmd

	switch msg := msg.(type) {
	case tea.KeyMsg:
		switch msg.String() {
		case ";":
			if c.upper.Focused() {
				c.upper.Blur()
				c.lower.Focus()
			} else {
				c.upper.Focus()
				c.lower.Blur()
			}
		default:
			if c.upper.Focused() {
				c.upper, cmd = c.upper.Update(msg)
			} else {
				c.lower, cmd = c.lower.Update(msg)
			}
		}
	}
	return c, cmd
}

func (c *complexTable) View() string {
	var (
		tableStyleActive            table.Styles
		styleActive                lipgloss.Style
		tableStyleInactive          table.Styles
		styleInactive              lipgloss.Style
		tableStyleInactiveNoSelect  table.Styles
	)
	if c.isBalanceSheet {
		tableStyleActive, styleActive = bsTblStyleActive()
		tableStyleInactive, styleInactive = bsTblStyleInactive()
	} else {
		tableStyleActive, styleActive = tblStyleActive()
		tableStyleInactive, styleInactive = tblStyleInactive()
		tableStyleInactiveNoSelect, _ = tblStyleInactiveNoSelect()
	}

	nonInteractiveTableStyle := table.DefaultStyles()
	nonInteractiveTableStyle.Header = nonInteractiveTableStyle.Header.
		BorderStyle(lipgloss.NormalBorder()).
		BorderForeground(lipgloss.Color("240")).
		BorderBottom(true).
		Bold(false)
	c.bottomBar.SetStyles(nonInteractiveTableStyle)

	// Apply row highlight styles:
	// - Income Statement: highlight ONLY the focused pane; non-focused has no row highlight
	// - Balance Sheet: preserve original behavior (no highlight for selected rows via bs styles)
	if c.isBalanceSheet {
		if c.upper.Focused() {
			c.upper.SetStyles(tableStyleActive)
			c.lower.SetStyles(tableStyleInactive)
		} else {
			c.upper.SetStyles(tableStyleInactive)
			c.lower.SetStyles(tableStyleActive)
		}
	} else {
		// Income Statement: focused pane active, non-focused no-select
		if c.upper.Focused() {
			c.upper.SetStyles(tableStyleActive)
			c.lower.SetStyles(tableStyleInactiveNoSelect)
		} else {
			c.upper.SetStyles(tableStyleInactiveNoSelect)
			c.lower.SetStyles(tableStyleActive)
		}
	}

	var upper, lower string
	if c.upper.Focused() {
		upper = styleActive.Render(c.upper.View())
		lower = styleInactive.Render(c.lower.View())
	} else {
		upper = styleInactive.Render(c.upper.View())
		lower = styleActive.Render(c.lower.View())
	}

	var mid string
	if c.horizontalLayout {
		// Render Revenues (upper) on the left and Expenses (lower) on the right
		mid = lipgloss.JoinHorizontal(lipgloss.Top, upper, lower)
	} else {
		mid = lipgloss.JoinVertical(lipgloss.Left, upper, lower)
	}

	return lipgloss.JoinVertical(
		lipgloss.Left,
		lipgloss.JoinVertical(
			lipgloss.Center,
			lipgloss.NewStyle().Bold(true).Render(c.title),
			mid,
		),
		styleInactive.Render(c.bottomBar.View()),
	)
}

func updateComplexTable(complexTable *complexTable, data *interfaces.ComplexTable, width int) {
	complexTable.title = data.Title
	complexTable.upperTitle = data.UpperTitle
	complexTable.lowerTitle = data.LowerTitle

	// Reset existing rows before applying new data
	complexTable.upper.SetRows(nil)
	complexTable.lower.SetRows(nil)
	complexTable.bottomBar.SetRows(nil)

	// Keep a copy of the incoming headers
	complexTable.columns = data.Columns

	// Compute the set of value columns we will render, mirroring setColumns logic.
	// We always show a leading Account column (from row[0]) and then a subset of
	// value columns derived from the CSV headers, skipping the first header cell
	// when it is an "Account" label and filtering out any "Commodity" column.
	allowedValueIdxs := make([]int, 0)
	if len(data.Columns) > 0 {
		startIdx := 0
		if strings.EqualFold(strings.TrimSpace(data.Columns[0]), "account") {
			startIdx = 1
		}
		for i := startIdx; i < len(data.Columns); i++ {
			if strings.EqualFold(strings.TrimSpace(data.Columns[i]), "commodity") {
				continue
			}
			allowedValueIdxs = append(allowedValueIdxs, i)
		}
	}

	// Set columns (sizes/titles) before setting rows so the table knows how many columns exist.
	setColumns(complexTable, width)

	// Project incoming rows to exactly match our configured columns:
	// - Keep the first cell (account name)
	// - Append only the value cells referenced by allowedValueIdxs
	projectRow := func(src []string) table.Row {
		if src == nil {
			return nil
		}
		out := make(table.Row, 0, 1+len(allowedValueIdxs))
		// Account/name column comes first
		if len(src) > 0 {
			out = append(out, src[0])
		} else {
			out = append(out, "")
		}
		// Then the selected value columns
		for _, idx := range allowedValueIdxs {
			if idx >= 0 && idx < len(src) {
				out = append(out, src[idx])
			} else {
				out = append(out, "")
			}
		}
		return out
	}

	upperRows := make([]table.Row, 0, len(data.Upper))
	for _, row := range data.Upper {
		upperRows = append(upperRows, projectRow(row))
	}
	complexTable.upper.SetRows(upperRows)

	lowerRows := make([]table.Row, 0, len(data.Lower))
	for _, row := range data.Lower {
		lowerRows = append(lowerRows, projectRow(row))
	}
	complexTable.lower.SetRows(lowerRows)

	// Bottom bar: project to keep counts aligned
	if data.BottomBar != nil {
		complexTable.bottomBar.SetRows([]table.Row{projectRow(data.BottomBar)})
	} else {
		complexTable.bottomBar.SetRows(nil)
	}
}

func setColumns(complexTable *complexTable, width int) {
	if len(complexTable.columns) == 0 {
		// it's possible for this method to be called
		// before data has been set
		return
	}

	// Build value columns by skipping the first header cell if it's an "Account"-like label
	// and filtering out any "Commodity" column if present
	raw := complexTable.columns
	valueCols := make([]string, 0)
	startIdx := 0
	if len(raw) > 0 {
		// Many hledger CSVs put "Account" as the first header cell
		if strings.EqualFold(strings.TrimSpace(raw[0]), "account") {
			startIdx = 1
		}
	}
	for _, c := range raw[startIdx:] {
		if strings.EqualFold(strings.TrimSpace(c), "commodity") {
			// Drop commodity column from reports
			continue
		}
		valueCols = append(valueCols, c)
	}

	accountColWidth := percent(width, 20)
	// Distribute remaining width evenly across value columns
	remainingWidth := width - accountColWidth - 2
	perColWidth := 0
	if len(valueCols) > 0 {
		perColWidth = remainingWidth/len(valueCols) - 2
		if perColWidth < 8 { // avoid too-narrow columns
			perColWidth = 8
		}
	}

	cols := make([]table.Column, 0, len(valueCols))
	for _, c := range valueCols {
		cols = append(cols, table.Column{Title: c, Width: perColWidth})
	}

	upperCols := make([]table.Column, 0)
	upperCols = append(upperCols, table.Column{
		Title: complexTable.upperTitle,
		Width: accountColWidth,
	},
	)
	upperCols = append(upperCols, cols...)
	complexTable.upper.SetColumns(upperCols)

	lowerCols := []table.Column{
		{
			Title: complexTable.lowerTitle,
			Width: accountColWidth,
		},
	}
	lowerCols = append(lowerCols, cols...)
	complexTable.lower.SetColumns(lowerCols)

	netCols := []table.Column{
		{
			Title: "Net",
			Width: accountColWidth,
		},
	}
	netCols = append(netCols, cols...)
	complexTable.bottomBar.SetColumns(netCols)
}
