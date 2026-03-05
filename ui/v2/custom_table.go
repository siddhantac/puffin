package ui

import (
	"github.com/charmbracelet/bubbles/spinner"
	"github.com/charmbracelet/bubbles/table"
	tea "github.com/charmbracelet/bubbletea"
	"github.com/charmbracelet/lipgloss"
)

type customTable struct {
	table.Model
	ready         bool
	name          string
	title         string
	titleModifier string
	spinner       spinner.Model
	cols          []table.Column
}

// SetColumns overrides the embedded method to track columns locally so we can
// compute the actual rendered width (which includes per-cell padding).
func (c *customTable) SetColumns(cols []table.Column) {
	c.cols = cols
	c.Model.SetColumns(cols)
}

// renderedWidth returns the actual character width that table.Model.View() produces.
// Each cell is rendered with Padding(0,1) by default (1 char left + right per column).
func (c *customTable) renderedWidth() int {
	w := 0
	for _, col := range c.cols {
		w += col.Width + 2
	}
	return w
}

func newCustomTable(title string) *customTable {
	return &customTable{
		Model:   table.New(),
		title:   title,
		spinner: newSpinner(),
	}
}

func (c *customTable) Init() tea.Cmd {
	return c.spinner.Tick
}

func (c *customTable) Update(msg tea.Msg) (*customTable, tea.Cmd) {
	var cmd tea.Cmd
	switch msg := msg.(type) {
	case spinner.TickMsg:
		c.spinner, cmd = c.spinner.Update(msg)
		return c, cmd
	}

	c.Model, cmd = c.Model.Update(msg)
	return c, cmd
}

func (c *customTable) View() string {
	tblStyleActive, styleActive := tableStyleActive()
	tblStyleInactive, styleInactive := tableStyleInactive()
	tblStyleUnready := tableStyleUnready()

	var (
		style      lipgloss.Style
		tableStyle table.Styles
	)

	if c.Model.Focused() {
		style = styleActive
	} else {
		style = styleInactive
	}

	var content string
	title := " " + c.title + c.titleModifier
	if !c.ready {
		tableStyle = tblStyleUnready
		tblW := c.renderedWidth()
		if tblW == 0 {
			tblW = c.Model.Width()
		}
		tblH := c.Model.Height() + 2 // +2 for header text line and header border line
		sty := lipgloss.NewStyle().
			Width(tblW).
			Height(tblH).
			Align(lipgloss.Center, lipgloss.Center).
			Render(c.spinner.View())
		content = style.Render(sty)
	} else {
		if c.Model.Focused() {
			tableStyle = tblStyleActive
		} else {
			tableStyle = tblStyleInactive
		}
		content = style.Render(c.Model.View())
	}

	c.Model.SetStyles(tableStyle)

	if c.title != "" {
		return lipgloss.JoinVertical(
			lipgloss.Left,
			title,
			content,
		)
	}

	return content

}

func (c *customTable) Ready() bool {
	return c.ready
}

func (c *customTable) SetReady(ready bool) {
	c.ready = ready
}

func (c *customTable) SetTitleModifier(s string) {
	c.titleModifier = s
}
