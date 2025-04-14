package ui

import (
	"strings"

	"github.com/siddhantac/puffin/ui/colorscheme"
	"github.com/siddhantac/puffin/ui/keys"

	"github.com/charmbracelet/lipgloss"
	"github.com/mistakenelf/teacup/help"
)

func newHelp() help.Model {
	entries := []help.Entry{
		{Key: strings.Join(keys.Up.Keys(), "/"), Description: "up"},
		{Key: strings.Join(keys.Down.Keys(), "/"), Description: "down"},
		{Key: strings.Join(keys.ScrollUp.Keys(), "/"), Description: "scroll up"},
		{Key: strings.Join(keys.ScrollDown.Keys(), "/"), Description: "scroll down"},

		{Key: strings.Join(keys.Refresh.Keys(), "/"), Description: "refresh"},
		{Key: strings.Join(keys.Filter.Keys(), "/"), Description: "filter"},
		{Key: strings.Join(keys.ResetFilters.Keys(), "/"), Description: "reset filters"},

		{Key: strings.Join(keys.AcctDepthIncr.Keys(), "/"), Description: "increase account depth"},
		{Key: strings.Join(keys.AcctDepthDecr.Keys(), "/"), Description: "decrease account depth"},
		{Key: strings.Join(keys.TreeView.Keys(), "/"), Description: "enable tree view"},
		{Key: strings.Join(keys.SortBy.Keys(), "/"), Description: "toggle sorting (amount/account)"},
		{Key: strings.Join(keys.ShowGraph.Keys(), "/"), Description: "toggle graph"},
		{Key: strings.Join(keys.Weekly.Keys(), "/"), Description: "weekly report"},
		{Key: strings.Join(keys.Monthly.Keys(), "/"), Description: "monthly report"},
		{Key: strings.Join(keys.Quarterly.Keys(), "/"), Description: "quarterly report"},
		{Key: strings.Join(keys.Yearly.Keys(), "/"), Description: "yearly report"},

		{Key: strings.Join(keys.Quit.Keys(), "/"), Description: "quit"},
		{Key: strings.Join(keys.Esc.Keys(), "/"), Description: "esc"},
		{Key: strings.Join(keys.Help.Keys(), "/"), Description: "help"},
	}

	return help.New(
		false,
		true,
		"Help",
		help.TitleColor{
			Background: lipgloss.AdaptiveColor{Light: "62", Dark: colorscheme.Nord0},
			Foreground: lipgloss.AdaptiveColor{Light: "#ffffff", Dark: colorscheme.Nord11},
		},
		lipgloss.AdaptiveColor{Light: "62", Dark: "62"},
		entries,
	)
}
