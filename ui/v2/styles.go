package ui

import (
	"github.com/charmbracelet/bubbles/table"
	"github.com/charmbracelet/lipgloss"
	"github.com/siddhantac/puffin/ui/colorscheme"
)

var (
	activeTitleStyle   = lipgloss.NewStyle().Bold(true).Background(lipgloss.Color(colorscheme.DraculaPink)).PaddingLeft(1).PaddingRight(1)
	inactiveTitleStyle = lipgloss.NewStyle().Bold(true).PaddingLeft(1).PaddingRight(1)
)

func tblStyleActive() (table.Styles, lipgloss.Style) {
	s := table.DefaultStyles()
	s.Header = s.Header.
		BorderStyle(lipgloss.NormalBorder()).
		BorderForeground(lipgloss.Color(colorscheme.DraculaComment)).
		BorderBottom(true).
		Bold(false)
	s.Selected = s.Selected.
		Foreground(lipgloss.Color(colorscheme.DraculaForeground)).
		Background(lipgloss.Color(colorscheme.DraculaPink)).
		Bold(false)
	return s, lipgloss.NewStyle().Border(lipgloss.RoundedBorder()).BorderForeground(lipgloss.Color(colorscheme.DraculaForeground))
}

func tblStyleInactive() (table.Styles, lipgloss.Style) {
	s := table.DefaultStyles()
	s.Header = s.Header.
		BorderStyle(lipgloss.NormalBorder()).
		BorderForeground(lipgloss.Color(colorscheme.DraculaComment)).
		BorderBottom(true).
		Bold(false)
	s.Selected = s.Selected.
		Foreground(lipgloss.Color(colorscheme.DraculaForeground)).
		Background(lipgloss.Color(colorscheme.DraculaCurrentLine)).
		Bold(false)
	return s, lipgloss.NewStyle().Border(lipgloss.RoundedBorder()).BorderForeground(lipgloss.Color(colorscheme.DraculaComment))
}
