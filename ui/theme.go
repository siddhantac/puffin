package ui

import (
	"github.com/siddhantac/puffin/ui/colorscheme"

	"github.com/charmbracelet/lipgloss"
)

type Theme struct {
	PrimaryBackground   lipgloss.Color
	SecondaryBackground lipgloss.Color
	PrimaryForeground   lipgloss.Color
	SecondaryForeground lipgloss.Color
	Accent              lipgloss.Color

	PrimaryColor   lipgloss.Color
	SecondaryColor lipgloss.Color
}

func ThemeNord() Theme {
	return Theme{
		PrimaryBackground:   lipgloss.Color(colorscheme.Nord0),
		PrimaryForeground:   lipgloss.Color(colorscheme.Nord1),
		SecondaryBackground: lipgloss.Color(colorscheme.Nord10),
		SecondaryForeground: lipgloss.Color(colorscheme.Nord9),
		Accent:              lipgloss.Color(colorscheme.Nord11),

		PrimaryColor:   lipgloss.Color(colorscheme.Nord1),
		SecondaryColor: lipgloss.Color(colorscheme.Nord9),
	}
}
