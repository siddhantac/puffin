package ui

import (
	"github.com/charmbracelet/bubbles/table"
	"github.com/charmbracelet/lipgloss"
	"github.com/siddhantac/puffin/ui/colorscheme"
)

var (
	activeTitleStyle   = lipgloss.NewStyle().Bold(true).Background(lipgloss.Color(colorscheme.GruvboxSkyBlue)).PaddingLeft(1).PaddingRight(1)
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
		Background(lipgloss.Color(colorscheme.GruvboxSkyBlue)).
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

// tblStyleInactiveNoSelect mirrors tblStyleInactive but disables row selection highlight
// by making Selected look like a normal cell. Useful for non-focused panes.
func tblStyleInactiveNoSelect() (table.Styles, lipgloss.Style) {
	s := table.DefaultStyles()
	s.Header = s.Header.
		BorderStyle(lipgloss.NormalBorder()).
		BorderForeground(lipgloss.Color(colorscheme.DraculaComment)).
		BorderBottom(true).
		Bold(false)
	// No highlight when not focused
	s.Selected = s.Cell
	return s, lipgloss.NewStyle().Border(lipgloss.RoundedBorder()).BorderForeground(lipgloss.Color(colorscheme.DraculaComment))
}

// bsTblStyleActive returns styles for Balance Sheet tables:
// - Header text in DraculaPurple (matches left panel purple)
// - No selection highlight (selected row looks like normal row)
func bsTblStyleActive() (table.Styles, lipgloss.Style) {
	s := table.DefaultStyles()
	s.Header = s.Header.
		BorderStyle(lipgloss.NormalBorder()).
		BorderForeground(lipgloss.Color(colorscheme.DraculaComment)).
		BorderBottom(true).
		Bold(false).
		Foreground(lipgloss.Color(colorscheme.DraculaPurple))
	// Make selected look like normal cell (no highlight)
	s.Selected = s.Cell
	return s, lipgloss.NewStyle().Border(lipgloss.RoundedBorder()).BorderForeground(lipgloss.Color(colorscheme.DraculaForeground))
}

// bsTblStyleInactive mirrors inactive table border while keeping purple headers
// and no selection highlight for Balance Sheet tables.
func bsTblStyleInactive() (table.Styles, lipgloss.Style) {
	s := table.DefaultStyles()
	s.Header = s.Header.
		BorderStyle(lipgloss.NormalBorder()).
		BorderForeground(lipgloss.Color(colorscheme.DraculaComment)).
		BorderBottom(true).
		Bold(false).
		Foreground(lipgloss.Color(colorscheme.DraculaPurple))
	// Make selected look like normal cell (no highlight)
	s.Selected = s.Cell
	return s, lipgloss.NewStyle().Border(lipgloss.RoundedBorder()).BorderForeground(lipgloss.Color(colorscheme.DraculaComment))
}
