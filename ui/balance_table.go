package ui

import (
	"github.com/charmbracelet/bubbles/table"
	tea "github.com/charmbracelet/bubbletea"
)

type balanceTable struct {
	*table.Model
	width             int
	height            int
	columnPercentages []int
}

func newBalanceTable() *balanceTable {
	return &balanceTable{
		columnPercentages: []int{50, 50},
		Model:             &table.Model{},
	}
}

func (b *balanceTable) SetColumns(firstRow table.Row) {
	if len(b.columnPercentages) != len(firstRow) {
		panic("length not equal")
	}

	cols := make([]table.Column, 0, len(firstRow))
	for i, row := range firstRow {
		c := table.Column{Title: row, Width: percent(b.width, b.columnPercentages[i])}
		cols = append(cols, c)
	}
	b.Model = newDefaultTable(cols)
    b.Model.SetHeight(b.height)
    b.Model.SetWidth(b.width)
}

func (b *balanceTable) SetWidth(width int) {
	b.width = width
	b.Model.SetWidth(width)
}

func (b *balanceTable) SetHeight(height int) {
	b.height = height
	b.Model.SetHeight(height)
}

func (b *balanceTable) Init() tea.Cmd {
	return nil
}

func (b *balanceTable) Update(msg tea.Msg) (tea.Model, tea.Cmd) {
	switch msg := msg.(type) {
	case balanceData: // set table data when it changes
		b.SetColumns(msg.Columns)
		b.Model.SetRows(msg.Rows)
	}
	b.Model.Update(msg)
	return b, nil
}

func (b *balanceTable) View() string {
	if b.Model == nil {
		return ""
	}
	return b.Model.View()
}
