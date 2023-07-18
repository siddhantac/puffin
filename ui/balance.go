package ui

import (
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
	// cols := []table.Column{
	// 	{Title: "name", Width: percent(width, 50)},
	// 	{Title: "amount", Width: percent(width, 50)},
	// }
	// b.Model = newDefaultTable(cols)
}

func (b *balanceTable) SetColumns2(firstRow table.Row) {
	percentages := []int{50, 50}
	if len(percentages) != len(firstRow) {
		panic("length not equal")
	}

	cols := make([]table.Column, 0, len(firstRow))
	for i, row := range firstRow {
		c := table.Column{Title: row, Width: percent(b.width, percentages[i])}
		cols = append(cols, c)
	}
	b.Model = newDefaultTable(cols)
}

func (b *balanceTable) SetWidth(width int) {
	b.width = width
	b.Model.SetWidth(width)
}

func (b *balanceTable) Init() tea.Cmd {
	return nil
}

func (b *balanceTable) Update(msg tea.Msg) (tea.Model, tea.Cmd) {
	switch msg := msg.(type) {
	case balanceData: // set table data when it changes
		b.SetColumns2(msg[0])
		b.Model.SetRows(msg[1:])
	}
	b.Model.Update(msg)
	return b, nil
}

func (b *balanceTable) View() string {
	return b.Model.View()
}
