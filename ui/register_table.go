package ui

import (
	"github.com/charmbracelet/bubbles/table"
	tea "github.com/charmbracelet/bubbletea"
)

type registerTable struct {
	table.Model
	width             int
	columnPercentages []int
	columns           []table.Column
}

func newRegisterTable() *registerTable {
	return &registerTable{
		columnPercentages: []int{10, 15, 30, 30, 15},
	}
}

func (r *registerTable) SetColumns(width int) {
}

func (r *registerTable) CalculateColumns(firstRow table.Row) []table.Column {
	if len(r.columnPercentages) != len(firstRow) {
		panic("length not equal")
	}

	cols := make([]table.Column, 0, len(firstRow))
	for i, row := range firstRow {
		c := table.Column{Title: row, Width: percent(r.width, r.columnPercentages[i])}
		cols = append(cols, c)
	}
	return cols
}

func (r *registerTable) SetWidth(width int) {
	r.width = width
	r.Model.SetWidth(width)
}

func (r *registerTable) Init() tea.Cmd {
	return nil
}

func (r *registerTable) Update(msg tea.Msg) (tea.Model, tea.Cmd) {
	switch msg := msg.(type) {
	case registerData: // set table data when it changes
		columns := r.CalculateColumns(msg.Columns)
		if len(columns) != len(r.columns) {
			r.columns = columns
			r.Model = newDefaultTable(r.columns)
		}
		r.Model.SetRows(msg.Rows)
	}
	r.Model.Update(msg)
	return r, nil
}

func (r *registerTable) View() string {
	return r.Model.View()
}