package ui

import "github.com/charmbracelet/bubbles/key"

// keyMap defines a set of keybindings. To work for help it must satisfy
// key.Map. It could also very easily be a map[string]key.Binding.
type keyMap struct {
	Up, Down             key.Binding
	Left, Right          key.Binding
	ScrollUp, ScrollDown key.Binding

	Help    key.Binding
	Refresh key.Binding
	Quit    key.Binding
	Esc     key.Binding

	Filter       key.Binding
	ResetFilters key.Binding

	AcctDepthDecr key.Binding
	AcctDepthIncr key.Binding
	TreeView      key.Binding

	Monthly   key.Binding
	Quarterly key.Binding
	Yearly    key.Binding
}

var allKeys = keyMap{
	Monthly: key.NewBinding(
		key.WithKeys("m", "m"),
		key.WithHelp("m", "monthly period"),
	),
	Quarterly: key.NewBinding(
		key.WithKeys("u", "u"),
		key.WithHelp("u", "quarterly period"),
	),
	Yearly: key.NewBinding(
		key.WithKeys("y", "y"),
		key.WithHelp("y", "yearly period"),
	),
	Up: key.NewBinding(
		key.WithKeys("up", "k"),
		key.WithHelp("↑/k", "move up"),
	),
	Down: key.NewBinding(
		key.WithKeys("down", "j"),
		key.WithHelp("↓/j", "move down"),
	),
	Left: key.NewBinding(
		key.WithKeys("left", "h"),
		key.WithHelp("←/h", "prev tab"),
	),
	Right: key.NewBinding(
		key.WithKeys("right", "l"),
		key.WithHelp("→/l", "next tab"),
	),
	Help: key.NewBinding(
		key.WithKeys("?"),
		key.WithHelp("?", "toggle help"),
	),
	Quit: key.NewBinding(
		key.WithKeys("q", "ctrl+c"),
		key.WithHelp("q", "quit"),
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
		key.WithHelp("esc", "escape"),
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
	TreeView: key.NewBinding(
		key.WithKeys("t"),
		key.WithHelp("t", "toggle tree mode"),
	),
	ScrollDown: key.NewBinding(
		key.WithKeys("shift+down", "shift+j"),
		key.WithHelp("J", "scroll down"),
	),
	ScrollUp: key.NewBinding(
		key.WithKeys("shift+up", "shift+k"),
		key.WithHelp("K", "scroll up"),
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
		{k.ScrollDown, k.ScrollUp},
		{k.Up, k.Down},
		{k.Filter, k.ResetFilters},
		{k.AcctDepthIncr, k.AcctDepthDecr},
		{k.Quarterly, k.TreeView},
		{k.Monthly, k.Yearly},
		{k.Refresh, k.ResetFilters},
		{k.Esc, k.Quit},
	}
}
