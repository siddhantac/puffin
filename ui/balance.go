package ui

import (
	"puffin/logger"

	"github.com/charmbracelet/bubbles/table"
	tea "github.com/charmbracelet/bubbletea"
)

type balanceTable struct {
	// *Table
	table.Model
	width int
}

func newBalanceTable( /* width int */ ) *balanceTable {
	b := balanceTable{}
	// b.Table = NewTable(width, b.Columns)
	return &b
}

func (b *balanceTable) SetColumns(width int) {
	cols := []table.Column{
		{Title: "name", Width: percent(width, 50)},
		{Title: "amount", Width: percent(width, 50)},
	}
	b.Model = newTable(cols)
	setTableStyle(b.Model)
}

func (b *balanceTable) Init() tea.Cmd {
	return nil
}

func (b *balanceTable) Update(msg tea.Msg) (tea.Model, tea.Cmd) {
	switch msg := msg.(type) {
	case accountsData: // set table data when it changes
		logger.Logf("accounts data: %d", len(msg))
		logger.Logf("accounts data: %d", len(msg[0]))
		b.Model.SetRows(msg)
	}
	b.Model.Update(msg)
	return b, nil
}

func (b *balanceTable) View() string {
	return b.Model.View()
}
