package ui

import (
	"fmt"

	"github.com/siddhantac/puffin/ui/colorscheme"

	"github.com/charmbracelet/lipgloss"
)

func header() string {
	return lipgloss.NewStyle().
		Bold(true).
		Background(lipgloss.Color(colorscheme.Nord0)).
		Foreground(theme.SecondaryColor).
		MarginBottom(1).
		PaddingLeft(7).
		PaddingRight(7).
		Render(fmt.Sprintf("Puffin %s", Version))
}
