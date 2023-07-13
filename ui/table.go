package ui

import (
	"github.com/charmbracelet/bubbles/table"
	"github.com/charmbracelet/lipgloss"
)

func balanceColumns() []table.Column {
	return []table.Column{
		{Title: "name", Width: 25},
		{Title: "amount", Width: 15},
	}
}

func percent(number, percentage int) int {
	return (percentage * number) / 100
}

func newTable(columns []table.Column) table.Model {
	t := table.New(
		table.WithColumns(columns),
		table.WithKeyMap(table.DefaultKeyMap()),
		table.WithFocused(true),
	)

	s := table.DefaultStyles()
	s.Header = s.Header.
		BorderStyle(lipgloss.NormalBorder()).
		BorderForeground(theme.SecondaryForeground).
		BorderTop(true).
		BorderBottom(true).
		Bold(false)
	s.Selected = s.Selected.
		Foreground(theme.PrimaryForeground).
		Background(theme.Accent).
		Bold(false)
	t.SetStyles(s)

	return t
}
