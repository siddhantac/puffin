package ui

import (
	"github.com/charmbracelet/bubbles/table"
	"github.com/charmbracelet/lipgloss"
)

var theme = ThemeNord()

var activeTableStyle = lipgloss.NewStyle().
	BorderStyle(lipgloss.HiddenBorder()).
	BorderForeground(lipgloss.Color("240"))

var inactiveTableStyle = lipgloss.NewStyle().
	BorderStyle(lipgloss.NormalBorder()).
	BorderForeground(lipgloss.Color("240"))

var titleTextStyle = lipgloss.NewStyle().
	Bold(true).
	Background(lipgloss.Color("55")).
	PaddingLeft(1).
	PaddingRight(1).
	MarginTop(1)

var containerStyle = lipgloss.NewStyle() //.
// MarginLeft(1)

var tabSeparatorStyle = lipgloss.NewStyle().
	Foreground(theme.SecondaryForeground)

var tabStyle = lipgloss.NewStyle().
	PaddingLeft(1).
	PaddingRight(1)

var activeTabStyle = tabStyle.Copy().
	Bold(true).
	Background(lipgloss.Color(theme.SecondaryBackground))

var inactiveTabStyle = tabStyle.Copy().
	Bold(false)

func getTableStyle() table.Styles {
	// Selected: lipgloss.NewStyle().Bold(true).Foreground(lipgloss.Color("212")),
	// Header:   lipgloss.NewStyle().Bold(true).Padding(0, 1),
	// Cell:     lipgloss.NewStyle().Padding(0, 1),
	// s := table.DefaultStyles()
	s := table.Styles{}
	s.Header = s.Header.
		BorderStyle(lipgloss.NormalBorder()).
		BorderForeground(theme.SecondaryForeground).
		BorderTop(true).
		BorderBottom(true).
		Bold(true)
	s.Selected = s.Selected.
		Foreground(theme.PrimaryForeground).
		Background(theme.Accent).
		Bold(false)

	return s
}
