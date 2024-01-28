package ui

import "github.com/charmbracelet/bubbles/key"

// keyMap defines a set of keybindings. To work for help it must satisfy
// key.Map. It could also very easily be a map[string]key.Binding.
type keyMap struct {
	PrevTab key.Binding
	NextTab key.Binding
	Up      key.Binding
	Down    key.Binding
	Help    key.Binding
	Quit    key.Binding
	// Switch            key.Binding
	Refresh           key.Binding
	Esc               key.Binding
	Filter            key.Binding
	SwapSortingByDate key.Binding
	Search            key.Binding
	ResetFilters      key.Binding
	AcctDepthDecr     key.Binding
	AcctDepthIncr     key.Binding
	Yearly            key.Binding
	Monthly           key.Binding
}

var allKeys = keyMap{
	Monthly: key.NewBinding(
		key.WithKeys("m", "m"),
		key.WithHelp("m", "monthly"),
	),
	Yearly: key.NewBinding(
		key.WithKeys("y", "y"),
		key.WithHelp("y", "yearly"),
	),
	NextTab: key.NewBinding(
		key.WithKeys("tab", "J", "shift+down"),
		key.WithHelp("tab", "next tab"),
	),
	PrevTab: key.NewBinding(
		key.WithKeys("shift+tab", "K", "shift+up"),
		key.WithHelp("shift+tab", "prev tab"),
	),
	Up: key.NewBinding(
		key.WithKeys("up", "k"),
		key.WithHelp("↑/k", "scroll up"),
	),
	Down: key.NewBinding(
		key.WithKeys("down", "j"),
		key.WithHelp("↓/j", "scroll down"),
	),
	Help: key.NewBinding(
		key.WithKeys("?"),
		key.WithHelp("?", "toggle help"),
	),
	Quit: key.NewBinding(
		key.WithKeys("q", "ctrl+c"),
		key.WithHelp("q", "quit/unfocus filters"),
	),
	Refresh: key.NewBinding(
		key.WithKeys("r"),
		key.WithHelp("r", "refresh"),
	),
	Filter: key.NewBinding(
		key.WithKeys("f"),
		key.WithHelp("f", "filters"),
	),
	Esc: key.NewBinding(
		key.WithKeys("esc"),
		key.WithHelp("esc", "unfocus filters"),
	),
	SwapSortingByDate: key.NewBinding(
		key.WithKeys("s"),
		key.WithHelp("s", "sort by oldest/newest"),
	),
	Search: key.NewBinding(
		key.WithKeys("/"),
		key.WithHelp("/", "search"),
	),
	ResetFilters: key.NewBinding(
		key.WithKeys("x"),
		key.WithHelp("x", "reset filters"),
	),
	AcctDepthDecr: key.NewBinding(
		key.WithKeys("-"),
		key.WithHelp("-", "dec acct depth"),
	),
	AcctDepthIncr: key.NewBinding(
		key.WithKeys("+"),
		key.WithHelp("+", "inc acct depth"),
	),
}

// ShortHelp returns keybindings to be shown in the mini help view. It's part
// of the key.Map interface.
func (k keyMap) ShortHelp() []key.Binding {
	return []key.Binding{
		k.Help,
		k.Quit,
	}
}

// FullHelp returns keybindings for the expanded help view. It's part of the
// key.Map interface.
func (k keyMap) FullHelp() [][]key.Binding {
	return [][]key.Binding{
		{k.NextTab, k.PrevTab},
		{k.Up, k.Down},
		{k.Filter, k.ResetFilters},
		{k.AcctDepthIncr, k.AcctDepthDecr},
		{k.Monthly, k.Yearly},
		{k.Esc, k.Quit},
		{k.Refresh},
		// {k.Search},
		// {k.SwapSortingByDate},
	}
}
