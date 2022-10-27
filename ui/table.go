package ui

import (
	"github.com/charmbracelet/bubbles/table"
	"github.com/charmbracelet/lipgloss"
)

var tableStyle = lipgloss.NewStyle().
	BorderStyle(lipgloss.NormalBorder()).
	BorderForeground(lipgloss.Color("240"))

func initialColumns() []table.Column {
	return registerColumns()
}

func balanceColumns() []table.Column {
	return []table.Column{
		{Title: "name", Width: 40},
		{Title: "amount", Width: 15},
	}
}

func registerColumns() []table.Column {
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
