package ui

import (
	"github.com/charmbracelet/bubbles/table"
)

func percent(number, percentage int) int {
	return (percentage * number) / 100
}

func newDefaultTable(columns []table.Column) table.Model {
	tbl := table.New(
		table.WithColumns(columns),
		table.WithKeyMap(table.DefaultKeyMap()),
		table.WithFocused(true),
	)

	tbl.SetStyles(getTableStyle())
	return tbl
}
