package ui

import "github.com/charmbracelet/bubbles/table"

type genericTableData struct {
	rows    []table.Row
	columns table.Row
}

func newGenericTableData(rows []table.Row) genericTableData {
	return genericTableData{
		rows:    rows[1:],
		columns: rows[0],
	}
}

func (d genericTableData) Columns() table.Row {
	return d.columns
}

func (d genericTableData) Rows() []table.Row {
	return d.rows
}
