package ui

import (
	"github.com/charmbracelet/bubbles/table"
	"github.com/charmbracelet/lipgloss"
)

var currentTheme ThemeName = ThemeDraculaName
var theme = GetTheme(currentTheme)

// UpdateTheme changes the current theme and updates all styles
func UpdateTheme(newTheme ThemeName) {
	currentTheme = newTheme
	theme = GetTheme(currentTheme)
	// Update all the styles that depend on the theme
	activeTabStyle = tabStyle.Copy().
		Bold(true).
		Background(theme.SecondaryColor).
		Foreground(theme.PrimaryColor)

	inactiveTabStyle = tabStyle.Copy().
		Bold(false).
		Foreground(theme.SecondaryColor)

	tabGroupStyle = lipgloss.NewStyle().
		MarginRight(1).
		MarginLeft(1).
		PaddingBottom(1).
		BorderStyle(lipgloss.NormalBorder()).
		BorderForeground(theme.PrimaryForeground).
		BorderBottom(true)

	sectionTitleStyle = lipgloss.NewStyle().
		MarginRight(1).
		PaddingRight(1).
		PaddingLeft(1).
		Foreground(theme.Accent)
}

// GetCurrentTheme returns the current theme name
func GetCurrentTheme() ThemeName {
	return currentTheme
}

var activeItemStyle = lipgloss.NewStyle().
	BorderStyle(lipgloss.NormalBorder()).
	BorderForeground(lipgloss.Color("240")).
	BorderLeft(true).
	PaddingLeft(1)

var tabStyle = lipgloss.NewStyle().
	PaddingLeft(1).
	PaddingRight(1)

var activeTabStyle = tabStyle.Copy().
	Bold(true).
	Background(theme.SecondaryColor).
	Foreground(theme.PrimaryColor)

var inactiveTabStyle = tabStyle.Copy().
	Bold(false).
	Foreground(theme.SecondaryColor)

var tabGroupStyle = lipgloss.NewStyle().
	MarginRight(1).
	MarginLeft(1).
	PaddingBottom(1).
	BorderStyle(lipgloss.NormalBorder()).
	BorderForeground(theme.PrimaryForeground).
	BorderBottom(true)

var sectionTitleStyle = lipgloss.NewStyle().
	MarginRight(1).
	PaddingRight(1).
	PaddingLeft(1).
	Foreground(theme.Accent)

func getTableStyle() table.Styles {
	// Selected: lipgloss.NewStyle().Bold(true).Foreground(lipgloss.Color("212")),
	// Header:   lipgloss.NewStyle().Bold(true).Padding(0, 1),
	// Cell:     lipgloss.NewStyle().Padding(0, 1),
	s := table.DefaultStyles()
	// s := table.Styles{}
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
