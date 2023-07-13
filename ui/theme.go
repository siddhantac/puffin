package ui

import (
	"puffin/ui/colorscheme"

	"github.com/charmbracelet/lipgloss"
)

type Theme struct {
	PrimaryBackground   lipgloss.Color
	SecondaryBackground lipgloss.Color
	PrimaryForeground   lipgloss.Color
	SecondaryForeground lipgloss.Color
	Accent              lipgloss.Color
}

func ThemeNord() Theme {
	return Theme{
		PrimaryBackground:   lipgloss.Color(colorscheme.Nord0),
		SecondaryBackground: lipgloss.Color(colorscheme.Nord2),
		PrimaryForeground:   lipgloss.Color(colorscheme.Nord6),
		SecondaryForeground: lipgloss.Color(colorscheme.Nord3),
		Accent:              lipgloss.Color(colorscheme.Nord11),
	}
}
