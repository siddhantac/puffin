package ui

import (
	"github.com/charmbracelet/bubbles/table"
	tea "github.com/charmbracelet/bubbletea"
	"github.com/charmbracelet/lipgloss"
)

type customTable struct {
	table.Model
	ready bool
	name  string
}

func newCustomTable() *customTable {
	return &customTable{
		Model: table.New(),
	}
}

func (c *customTable) Update(msg tea.Msg) (*customTable, tea.Cmd) {
	var cmd tea.Cmd
	c.Model, cmd = c.Model.Update(msg)
	return c, cmd
}

func (c *customTable) View() string {
	tblStyleActive, styleActive := tableStyleActive()
	tblStyleInactive, styleInactive := tableStyleInactive()
	tblStyleUnready := tableStyleUnready()

	var (
		view       string
		style      lipgloss.Style
		tableStyle table.Styles
	)

	if c.Model.Focused() {
		style = styleActive
	} else {
		style = styleInactive
	}

	if !c.ready {
		tableStyle = tblStyleUnready
	} else {
		if c.Model.Focused() {
			tableStyle = tblStyleActive
		} else {
			tableStyle = tblStyleInactive
		}
	}

	c.Model.SetStyles(tableStyle)
	view = style.Render(c.Model.View())

	return view
}

func (c *customTable) Ready() bool {
	return c.ready
}

func (c *customTable) SetReady(ready bool) {
	c.ready = ready
}
