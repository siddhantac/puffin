package ui

import (
	"github.com/charmbracelet/bubbles/table"
	tea "github.com/charmbracelet/bubbletea"
)

type expensesTable struct {
	*table.Model
	width  int
	height int
}

func newExpensesTable() *expensesTable {
	return &expensesTable{
		Model: &table.Model{},
	}
}

func (b *expensesTable) SetColumns(firstRow table.Row) {
	cols := make([]table.Column, 0, len(firstRow))
	cols = append(cols, table.Column{Title: firstRow[0], Width: percent(b.width, 20)})
	for _, row := range firstRow[1:] {
		c := table.Column{Title: row, Width: percent(b.width, 80/(len(firstRow)-1))}
		cols = append(cols, c)
	}
	b.Model = newDefaultTable(cols)
	b.Model.SetHeight(b.height)
	b.Model.SetWidth(b.width)
}

func (b *expensesTable) SetWidth(width int) {
	b.width = width
	b.Model.SetWidth(width)
}

func (b *expensesTable) SetHeight(height int) {
	b.height = height
	b.Model.SetHeight(height)
}

func (b *expensesTable) Init() tea.Cmd {
	return nil
}

func (b *expensesTable) Update(msg tea.Msg) (tea.Model, tea.Cmd) {
	switch msg := msg.(type) {
	case expensesData: // set table data when it changes
		b.SetColumns(msg.Columns)
		b.Model.SetRows(msg.Rows)
	}
	b.Model.Update(msg)
	return b, nil
}

func (b *expensesTable) View() string {
	if b.Model == nil {
		return ""
	}
	return b.Model.View()
}
