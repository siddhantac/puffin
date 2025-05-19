package ui

import (
	"github.com/charmbracelet/bubbles/table"
	tea "github.com/charmbracelet/bubbletea"
)

type customTable struct {
	table.Model
}

func newCustomTable() customTable {
	return customTable{
		Model: table.New(),
	}
}

func (c customTable) Update(msg tea.Msg) (customTable, tea.Cmd) {
	var cmd tea.Cmd
	c.Model, cmd = c.Model.Update(msg)
	return c, cmd
}

func (c customTable) View() string {
	tableStyleActive, styleActive := tblStyleActive()
	tableStyleInactive, styleInactive := tblStyleInactive()

	var view string

	if c.Model.Focused() {
		c.Model.SetStyles(tableStyleActive)
		view = styleActive.Render(c.Model.View())
	} else {
		c.Model.SetStyles(tableStyleInactive)
		view = styleInactive.Render(c.Model.View())
	}

	return view
}
