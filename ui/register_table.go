package ui

import (
	"puffin/accounting"

	"github.com/charmbracelet/bubbles/table"
	tea "github.com/charmbracelet/bubbletea"
)

type registerTable struct {
	*table.Model
	width             int
	height            int
	columnPercentages []int
	columns           []table.Column
}

func newRegisterTable() *registerTable {
	return &registerTable{
		columnPercentages: []int{5, 10, 30, 20, 15},
		Model:             &table.Model{},
	}
}

func (r *registerTable) SetColumns(firstRow table.Row) {
	if len(r.columnPercentages) != len(firstRow) {
		panic("length not equal")
	}

	cols := make([]table.Column, 0, len(firstRow))
	for i, row := range firstRow {
		c := table.Column{Title: row, Width: percent(r.width, r.columnPercentages[i])}
		cols = append(cols, c)
	}

	if len(cols) != len(r.columns) {
		r.columns = cols
		r.Model = newDefaultTable(cols)
		r.Model.SetHeight(r.height)
		r.Model.SetWidth(r.width)
	}
}

func (r *registerTable) SetWidth(width int) {
	r.width = width
	r.Model.SetWidth(width)
}

func (r *registerTable) SetHeight(height int) {
	r.height = height
	r.Model.SetHeight(height)
}

func (r *registerTable) Update(msg tea.Msg) (tea.Model, tea.Cmd) {
	switch msg := msg.(type) {
	case accounting.RegisterData: // set table data when it changes
		r.SetColumns(msg.Columns())
		r.Model.SetRows(msg.Rows())
	}
	r.Model.Update(msg)
	return r, nil
}
