package v2

import "github.com/charmbracelet/bubbles/key"

type KeyMap struct {
	Help    key.Binding
	Quit    key.Binding
	Down    key.Binding
	Up      key.Binding
	Refresh key.Binding
}

func (k KeyMap) ShortHelp() []key.Binding {
	return []key.Binding{k.Help, k.Quit}
}

func (k KeyMap) FullHelp() [][]key.Binding {
	return [][]key.Binding{
		{k.Help, k.Quit},
		{k.Up, k.Down},
		{k.Refresh},
	}
}

var Keys = KeyMap{
	Help:    key.NewBinding(key.WithKeys("?"), key.WithHelp("?", "toggle help")),
	Quit:    key.NewBinding(key.WithKeys("q", "esc", "ctrl+c"), key.WithHelp("q", "quit")),
	Up:      key.NewBinding(key.WithKeys("k", "up"), key.WithHelp("k", "up")),
	Down:    key.NewBinding(key.WithKeys("j", "down"), key.WithHelp("j", "down")),
	Refresh: key.NewBinding(key.WithKeys("r"), key.WithHelp("r", "refresh")),
}
