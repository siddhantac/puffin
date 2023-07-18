package ui

import (
	"github.com/charmbracelet/bubbles/table"
	tea "github.com/charmbracelet/bubbletea"
)

type incomeStatementTable struct {
	table.Model
	width int
}

func newIncomeStatementTable() *incomeStatementTable {
	return &incomeStatementTable{}
}

func (is *incomeStatementTable) SetColumns(width int) {
}

func (is *incomeStatementTable) SetColumns2(firstRow table.Row) {
	cols := make([]table.Column, 0, len(firstRow))
	for i, r := range firstRow {
		var w int
		if i == 0 {
			w = 25
		} else {
			w = 75 / len(firstRow)
		}

		c := table.Column{Title: r, Width: percent(is.width, w)}
		cols = append(cols, c)
	}

	is.Model = newDefaultTable(cols)
}

func (is *incomeStatementTable) Init() tea.Cmd {
	return nil
}

func (is *incomeStatementTable) SetWidth(w int) {
	is.width = w
	is.Model.SetWidth(w)
}

func (is *incomeStatementTable) Update(msg tea.Msg) (tea.Model, tea.Cmd) {
	switch msg := msg.(type) {
	case incomeStatementData:
		is.SetColumns2(msg.Columns)
		is.Model.SetRows(msg.Rows)
	}

	is.Model.Update(msg)
	return is, nil
}

func (is *incomeStatementTable) View() string {
	return is.Model.View()
}