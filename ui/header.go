package ui

import (
	"puffin/ui/colorscheme"

	"github.com/charmbracelet/lipgloss"
)

func header() string {
	return lipgloss.NewStyle().
		Bold(true).
		Background(lipgloss.Color(colorscheme.Nord0)).
		Foreground(theme.SecondaryColor).
		MarginTop(1).
		MarginBottom(1).
		PaddingLeft(7).
		PaddingRight(7).
		Render("Puffin")
}
