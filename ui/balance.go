package ui

import (
	"github.com/charmbracelet/bubbles/table"
	tea "github.com/charmbracelet/bubbletea"
)

type balanceTable struct {
	*Table
	width int
}

func newBalanceTable(width int) *balanceTable {
	b := balanceTable{}
	b.Table = NewTable(width, b.Columns)
	return &b
}

func (b *balanceTable) Columns(width int) []table.Column {
	width = width - 20
	return []table.Column{
		{Title: "name", Width: percent(width, 50)},
		{Title: "amount", Width: percent(width, 50)},
	}
}

func (b *balanceTable) Init() tea.Cmd {
	return nil
}

func (b *balanceTable) Update(msg tea.Msg) (tea.Model, tea.Cmd) {
	b.Table.Update(msg)
	return b, nil
}

func (b *balanceTable) View() string {
	return b.Model.View()
}
