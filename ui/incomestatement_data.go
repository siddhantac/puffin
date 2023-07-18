package ui

import "github.com/charmbracelet/bubbles/table"

type incomeStatementData struct {
	Rows    []table.Row
	Columns table.Row
}

func createIncomeStatementData(data [][]string) incomeStatementData {
	bd := incomeStatementData{
		Rows:    make([]table.Row, 0, len(data)),
		Columns: data[0],
	}

	for _, d := range data[1:] {
		bd.Rows = append(bd.Rows, d)
	}

	return bd
}
