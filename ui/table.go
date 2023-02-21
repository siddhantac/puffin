package ui

import (
	"github.com/charmbracelet/bubbles/table"
	"github.com/charmbracelet/lipgloss"
)

func initialColumns() []table.Column {
	return registerColumns()
}

func balanceColumns() []table.Column {
	return []table.Column{
		{Title: "name", Width: 25},
		{Title: "amount", Width: 15},
	}
}

func registerColumns() []table.Column {
	return []table.Column{
		{Title: "txnidx", Width: 5},
		{Title: "date", Width: 10},
		{Title: "description", Width: 30},
		{Title: "account", Width: 30},
		{Title: "amount", Width: 12},
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
