package ui

import "github.com/charmbracelet/bubbles/key"

// keyMap defines a set of keybindings. To work for help it must satisfy
// key.Map. It could also very easily be a map[string]key.Binding.
type keyMap struct {
	Up    key.Binding
	Down  key.Binding
	Left  key.Binding
	Right key.Binding
	Help  key.Binding
	Quit  key.Binding
	// Switch            key.Binding
	Refresh           key.Binding
	AccountFilter     key.Binding
	DateFilter        key.Binding
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
		key.WithHelp("m", "monthly period"),
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
		key.WithKeys("q", "esc", "ctrl+c"),
		key.WithHelp("q", "quit"),
	),
	// Switch: key.NewBinding(
	// 	key.WithKeys("left", "right"),
	// 	key.WithHelp("←/→", "switch table"),
	// ),
	Refresh: key.NewBinding(
		key.WithKeys("r"),
		key.WithHelp("r", "refresh"),
	),
	AccountFilter: key.NewBinding(
		key.WithKeys("a", "f"),
		key.WithHelp("a", "account filter"),
	),
	DateFilter: key.NewBinding(
		key.WithKeys("d"),
		key.WithHelp("d", "date filter"),
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
		{ /*k.Switch,*/ k.Refresh, k.Search},
		{k.AccountFilter, k.DateFilter, k.ResetFilters},
		{k.AcctDepthDecr, k.AcctDepthIncr, k.SwapSortingByDate},
	}
}
