package ui

import (
	"github.com/charmbracelet/bubbles/table"
	"github.com/charmbracelet/lipgloss"
)

func initialColumns() []table.Column {
	return []table.Column{
		{Title: "txnidx", Width: 10},
		{Title: "date", Width: 10},
		{Title: "description", Width: 40},
		{Title: "account", Width: 30},
		{Title: "amount", Width: 10},
	}
}

func buildTable(columns []table.Column) table.Model {
	t := table.New(
		table.WithColumns(columns),
		table.WithKeyMap(table.DefaultKeyMap()),
		table.WithFocused(true),
	)

	s := table.DefaultStyles()
	s.Header = s.Header.
		BorderStyle(lipgloss.NormalBorder()).
		BorderForeground(lipgloss.Color("240")).
		BorderBottom(true).
		Bold(false)
	s.Selected = s.Selected.
		Foreground(lipgloss.Color("229")).
		Background(lipgloss.Color("57")).
		Bold(false)
	t.SetStyles(s)

	return t
}

// var columnWidths = map[string]int{
// 	"txnidx":      10,
// 	"date":        10,
// 	"description": 40,
// 	"account":     30,
// 	"amount":      10,
// }
//
// func createHeader(headerRow []string) []table.Column {
// 	columns := make([]table.Column, 0)
// 	for _, c := range headerRow {
// 		if width, ok := columnWidths[c]; ok {
// 			col := table.Column{
// 				Title: c,
// 				Width: width,
// 			}
// 			columns = append(columns, col)
// 		}
// 	}
//
// 	return columns
// }

var baseStyle = lipgloss.NewStyle().
	BorderStyle(lipgloss.NormalBorder()).
	BorderForeground(lipgloss.Color("240"))
