package ui

import (
	"github.com/charmbracelet/bubbles/table"
	tea "github.com/charmbracelet/bubbletea"
)

type registerTable struct {
	table.Model
	width int
}

func newRegisterTable() *registerTable {
	return &registerTable{}
}

func (r *registerTable) SetColumns(width int) {
	cols := []table.Column{
		{Title: "txnidx", Width: percent(width, 10)},
		{Title: "date", Width: percent(width, 15)},
		{Title: "description", Width: percent(width, 30)},
		{Title: "account", Width: percent(width, 30)},
		{Title: "amount", Width: percent(width, 15)},
	}
	r.Model = newDefaultTable(cols)
}

func (r *registerTable) Init() tea.Cmd {
	return nil
}

func (r *registerTable) Update(msg tea.Msg) (tea.Model, tea.Cmd) {
	switch msg := msg.(type) {
	case transactionsData: // set table data when it changes
		r.Model.SetRows(msg)
	}
	r.Model.Update(msg)
	return r, nil
}

func (r *registerTable) View() string {
	return r.Model.View()
}
