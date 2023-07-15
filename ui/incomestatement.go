package ui

import (
	"github.com/charmbracelet/bubbles/table"
	tea "github.com/charmbracelet/bubbletea"
)

type incomeStatementTable struct {
	*Table
}

func newIncomeStatementTable() *incomeStatementTable {
	is := &incomeStatementTable{}
	return is
}

func (is *incomeStatementTable) Columns(width int, firstRow table.Row) func(int) []table.Column {
	return func(width int) []table.Column {
		cols := make([]table.Column, 0, len(firstRow))
		for i, r := range firstRow {
			var w int
			if i == 0 {
				w = 25
			} else {
				w = 75 / len(firstRow)
			}

			c := table.Column{Title: r, Width: percent(width, w)}
			cols = append(cols, c)
		}

		return cols
	}
}

func (is *incomeStatementTable) Update(msg tea.Msg) (tea.Model, tea.Cmd) {
	switch msg := msg.(type) {
	case incomeStatementData:
		is.Table = NewTable(200, is.Columns(200, msg[0]))
		is.SetRows(msg[1:])
	}

	if is.Table != nil {
		is.Table.Update(msg)
	}
	return is, nil
}
