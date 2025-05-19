package ui

import (
	"fmt"

	"github.com/charmbracelet/bubbles/spinner"
	"github.com/charmbracelet/bubbles/table"
	tea "github.com/charmbracelet/bubbletea"
	"github.com/charmbracelet/lipgloss"
)

type customTable struct {
	table.Model
	ready   bool
	name    string
	title   string
	spinner spinner.Model
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

	title := "  " + c.title
	if !c.ready {
		tableStyle = tblStyleUnready
		title = fmt.Sprintf("%s%s", c.spinner.View(), c.title)
	} else {
		if c.Model.Focused() {
			tableStyle = tblStyleActive
		} else {
			tableStyle = tblStyleInactive
		}
	}

	c.Model.SetStyles(tableStyle)

	if c.title != "" {
		return lipgloss.JoinVertical(
			lipgloss.Left,
			title,
			style.Render(c.Model.View()),
		)
	}

	return style.Render(c.Model.View())

}

func (c *customTable) Ready() bool {
	return c.ready
}

func (c *customTable) SetReady(ready bool) {
	c.ready = ready
}
