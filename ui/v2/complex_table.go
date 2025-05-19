package ui

import (
	"github.com/siddhantac/puffin/ui/v2/interfaces"

	"github.com/charmbracelet/bubbles/table"
	tea "github.com/charmbracelet/bubbletea"
	"github.com/charmbracelet/lipgloss"
)

type complexTable struct {
	title, upperTitle, lowerTitle string
	bottomBar                     table.Model
	upper, lower                  *customTable
	focus                         bool
	columns                       []string
}

func newComplexTable() *complexTable {
	ct := &complexTable{
		upper:     newCustomTable(""),
		lower:     newCustomTable(""),
		bottomBar: table.New(),
	}
	ct.upper.SetReady(true)
	ct.lower.SetReady(true)
	return ct
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
	nonInteractiveTableStyle := table.DefaultStyles()
	nonInteractiveTableStyle.Header = nonInteractiveTableStyle.Header.
		BorderStyle(lipgloss.NormalBorder()).
		BorderForeground(lipgloss.Color("240")).
		BorderBottom(true).
		Bold(false)
	c.bottomBar.SetStyles(nonInteractiveTableStyle)

	upper := c.upper.View()
	lower := c.lower.View()

	_, styleInactive := tableStyleInactive()

	return lipgloss.JoinVertical(
		lipgloss.Left,
		lipgloss.JoinVertical(
			lipgloss.Center,
			lipgloss.NewStyle().Bold(true).Render(c.title),
			upper,
		),
		lower,
		styleInactive.Render(c.bottomBar.View()),
	)
}

func updateComplexTable(complexTable *complexTable, data *interfaces.ComplexTable, width int) {
	complexTable.title = data.Title
	complexTable.upperTitle = data.UpperTitle
	complexTable.lowerTitle = data.LowerTitle

	complexTable.upper.SetRows(nil)
	complexTable.lower.SetRows(nil)
	complexTable.bottomBar.SetRows(nil)

	complexTable.columns = data.Columns

	setColumns(complexTable, width)

	upperRows := make([]table.Row, 0, len(data.Upper))
	for _, row := range data.Upper {
		upperRows = append(upperRows, row)
	}
	complexTable.upper.SetRows(upperRows)

	lowerRows := make([]table.Row, 0, len(data.Upper))
	for _, row := range data.Lower {
		lowerRows = append(lowerRows, row)
	}
	complexTable.lower.SetRows(lowerRows)

	complexTable.bottomBar.SetRows([]table.Row{data.BottomBar})
}

func setColumns(complexTable *complexTable, width int) {
	if len(complexTable.columns) == 0 {
		// it's possible for this method to be called
		// before data has been set
		return
	}

	cols := calculateColumns(complexTable.columns, width)

	upperCols := make([]table.Column, 0)
	upperCols = append(upperCols, table.Column{
		Title: complexTable.upperTitle,
		Width: cols[0].Width,
	},
	)
	upperCols = append(upperCols, cols[1:]...)
	complexTable.upper.SetColumns(upperCols)

	lowerCols := []table.Column{
		{
			Title: complexTable.lowerTitle,
			Width: cols[0].Width,
		},
	}
	lowerCols = append(lowerCols, cols[1:]...)
	complexTable.lower.SetColumns(lowerCols)

	netCols := []table.Column{
		{
			Title: "Net",
			Width: cols[0].Width,
		},
	}
	netCols = append(netCols, cols[1:]...)
	complexTable.bottomBar.SetColumns(netCols)
}

func calculateColumns(columnData []string, width int) []table.Column {
	accountColWidth := percent(width, 20)
	commodityColWidth := 10
	remainingWidth := width - accountColWidth - commodityColWidth - 2
	otherColumnsWidth := remainingWidth/(len(columnData)-2) - 2

	cols := []table.Column{
		{
			Title: "",
			Width: accountColWidth,
		},
		{
			Title: columnData[1],
			Width: commodityColWidth,
		},
	}
	for _, c := range columnData[2:] {
		cols = append(cols,
			table.Column{
				Title: c,
				Width: otherColumnsWidth,
			})
	}

	return cols
}
