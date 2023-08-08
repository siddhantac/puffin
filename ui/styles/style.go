package styles

import (
	"github.com/charmbracelet/bubbles/table"
	"github.com/charmbracelet/lipgloss"
)

var Theme = ThemeNord()

var ActiveTableStyle = lipgloss.NewStyle().
	BorderStyle(lipgloss.HiddenBorder()).
	BorderForeground(lipgloss.Color("240"))

var InactiveTableStyle = lipgloss.NewStyle().
	BorderStyle(lipgloss.NormalBorder()).
	BorderForeground(lipgloss.Color("240"))

var TitleTextStyle = lipgloss.NewStyle().
	Bold(true).
	Background(lipgloss.Color("55")).
	PaddingLeft(1).
	PaddingRight(1).
	MarginTop(1)

var ContainerStyle = lipgloss.NewStyle().PaddingLeft(1)

var TabSeparatorStyle = lipgloss.NewStyle().
	Foreground(Theme.SecondaryForeground)

var TabStyle = lipgloss.NewStyle().
	PaddingLeft(1).
	PaddingRight(1)

var ActiveTabStyle = TabStyle.Copy().
	Bold(true).
	Background(lipgloss.Color(Theme.SecondaryBackground))

var InactiveTabStyle = TabStyle.Copy().
	Bold(false)

var FilterPanelStyle = lipgloss.NewStyle().Border(lipgloss.RoundedBorder())

func GetTableStyle() table.Styles {
	// Selected: lipgloss.NewStyle().Bold(true).Foreground(lipgloss.Color("212")),
	// Header:   lipgloss.NewStyle().Bold(true).Padding(0, 1),
	// Cell:     lipgloss.NewStyle().Padding(0, 1),
	s := table.DefaultStyles()
	// s := table.Styles{}
	s.Header = s.Header.
		BorderStyle(lipgloss.NormalBorder()).
		BorderForeground(Theme.SecondaryForeground).
		BorderTop(true).
		BorderBottom(true).
		Bold(true)
	s.Selected = s.Selected.
		Foreground(Theme.PrimaryForeground).
		Background(Theme.Accent).
		Bold(false)

	return s
}
