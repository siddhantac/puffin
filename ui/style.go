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

func setTableStyle(tbl table.Model) {
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
	tbl.SetStyles(s)
}
