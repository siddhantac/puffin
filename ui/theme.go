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
		Accent:              lipgloss.Color(colorscheme.GruvboxSkyBlue),

		PrimaryColor:   lipgloss.Color(colorscheme.Nord1),
		SecondaryColor: lipgloss.Color(colorscheme.Nord9),
	}
}

func ThemeDracula() Theme {
	return Theme{
		PrimaryBackground:   lipgloss.Color(colorscheme.DraculaBackground),
		PrimaryForeground:   lipgloss.Color(colorscheme.DraculaForeground),
		SecondaryBackground: lipgloss.Color(colorscheme.DraculaCurrentLine),
		SecondaryForeground: lipgloss.Color(colorscheme.DraculaComment),
		Accent:              lipgloss.Color(colorscheme.GruvboxSkyBlue),

		PrimaryColor:   lipgloss.Color(colorscheme.DraculaForeground),
		SecondaryColor: lipgloss.Color(colorscheme.DraculaPurple),
	}
}

func ThemeGruvbox() Theme {
	return Theme{
		PrimaryBackground:   lipgloss.Color(colorscheme.GruvboxBackground),
		PrimaryForeground:   lipgloss.Color(colorscheme.GruvboxForeground),
		SecondaryBackground: lipgloss.Color(colorscheme.GruvboxBackgroundSoft),
		SecondaryForeground: lipgloss.Color(colorscheme.GruvboxGray),
		Accent:              lipgloss.Color(colorscheme.GruvboxSkyBlue),

		PrimaryColor:   lipgloss.Color(colorscheme.GruvboxForeground),
		SecondaryColor: lipgloss.Color(colorscheme.GruvboxBlueBright),
	}
}

// ThemeName represents the available themes
type ThemeName int

const (
	ThemeNordName ThemeName = iota
	ThemeDraculaName
	ThemeGruvboxName
)

// String returns the string representation of the theme name
func (t ThemeName) String() string {
	switch t {
	case ThemeNordName:
		return "nord"
	case ThemeDraculaName:
		return "dracula"
	case ThemeGruvboxName:
		return "gruvbox"
	default:
		return "nord"
	}
}

// GetTheme returns the theme based on the theme name
func GetTheme(themeName ThemeName) Theme {
	switch themeName {
	case ThemeNordName:
		return ThemeNord()
	case ThemeDraculaName:
		return ThemeDracula()
	case ThemeGruvboxName:
		return ThemeGruvbox()
	default:
		return ThemeDracula()
	}
}

// NextTheme cycles to the next theme
func NextTheme(current ThemeName) ThemeName {
	switch current {
	case ThemeNordName:
		return ThemeDraculaName
	case ThemeDraculaName:
		return ThemeGruvboxName
	case ThemeGruvboxName:
		return ThemeNordName
	default:
		return ThemeDraculaName
	}
}
