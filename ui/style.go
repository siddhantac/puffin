package ui

import "github.com/charmbracelet/lipgloss"

var activeTableStyle = lipgloss.NewStyle().
	MarginLeft(5).
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
