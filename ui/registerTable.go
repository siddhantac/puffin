package ui

import (
	"github.com/charmbracelet/bubbles/table"
	tea "github.com/charmbracelet/bubbletea"
)

type registerTable struct {
	*Table
	width int
}

func newRegisterTable(width int) *registerTable {
	r := registerTable{}
	r.Table = NewTable(width, r.Columns)
	return &r
}

func (r *registerTable) Columns(width int) []table.Column {
	width = width - 20
	return []table.Column{
		{Title: "txnidx", Width: percent(width, 10)},
		{Title: "date", Width: percent(width, 15)},
		{Title: "description", Width: percent(width, 30)},
		{Title: "account", Width: percent(width, 30)},
		{Title: "amount", Width: percent(width, 15)},
	}
}

func (r *registerTable) Init() tea.Cmd {
	return nil
}

func (r *registerTable) Update(msg tea.Msg) (tea.Model, tea.Cmd) {
	r.Table.Update(msg)
	return r, nil
}

func (r *registerTable) View() string {
	return r.Model.View()
}
