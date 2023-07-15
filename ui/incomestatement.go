package ui

import (
	"github.com/charmbracelet/bubbles/table"
	tea "github.com/charmbracelet/bubbletea"
)

type incomeStatementTable struct {
	*Table
}

func newIncomeStatementTable( /* width, numCols int */ ) *incomeStatementTable {
	is := &incomeStatementTable{}
	// is.Table = NewTable(width, is.Columns(width, numCols))
	return is
}

// func (is *incomeStatementTable) Columns(width int) []table.Column {
// 	return []table.Column{
// 		{Title: "item", Width: percent(width, 50)},
// 		{Title: "amount", Width: percent(width, 50)},
// 	}
// }

func (is *incomeStatementTable) Columns(width, numCols int) func(int) []table.Column {
	return func(width int) []table.Column {
		cols := []table.Column{
			{Title: "item", Width: percent(width, 25)},
		}

		amountCols := numCols - 1
		colWidth := 75 / amountCols
		c := table.Column{Title: "amount", Width: percent(width, colWidth)}
		for i := 0; i < amountCols; i++ {
			cols = append(cols, c)
		}

		return cols
	}
}

func (is *incomeStatementTable) Update(msg tea.Msg) (tea.Model, tea.Cmd) {
	switch msg := msg.(type) {
	case incomeStatementData:
		cols := len(msg[0])
		is.Table = NewTable(200, is.Columns(200, cols))
		is.SetRows(msg)
	}

	if is.Table != nil {
		is.Table.Update(msg)
	}
	return is, nil
}
