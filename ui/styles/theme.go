package styles

import (
	"puffin/ui/colorscheme"

	"github.com/charmbracelet/lipgloss"
)

type ThemeConfig struct {
	PrimaryBackground   lipgloss.Color
	SecondaryBackground lipgloss.Color
	PrimaryForeground   lipgloss.Color
	SecondaryForeground lipgloss.Color
	Accent              lipgloss.Color
}

func ThemeNord() ThemeConfig {
	return ThemeConfig{
		PrimaryBackground:   lipgloss.Color(colorscheme.Nord0),
		SecondaryBackground: lipgloss.Color(colorscheme.Nord2),
		PrimaryForeground:   lipgloss.Color(colorscheme.Nord6),
		SecondaryForeground: lipgloss.Color(colorscheme.Nord3),
		Accent:              lipgloss.Color(colorscheme.Nord11),
	}
}
