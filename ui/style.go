package ui

import "github.com/charmbracelet/lipgloss"

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

var containerStyle = lipgloss.NewStyle().
	MarginLeft(3)

var tabSeparatorStyle = lipgloss.NewStyle().
	Foreground(lipgloss.Color("240"))

var tabStyle = lipgloss.NewStyle().
	PaddingLeft(1).
	PaddingRight(1)

var activeTabStyle = tabStyle.Copy().
	Bold(true).
	Background(lipgloss.Color(theme.SecondaryBackground))

var inactiveTabStyle = tabStyle.Copy().
	Bold(false)
