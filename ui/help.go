package ui

import (
	"github.com/charmbracelet/bubbles/help"
	"github.com/charmbracelet/bubbles/key"
	tea "github.com/charmbracelet/bubbletea"
	"github.com/charmbracelet/lipgloss"
)

// keyMap defines a set of keybindings. To work for help it must satisfy
// key.Map. It could also very easily be a map[string]key.Binding.
type keyMap struct {
	Up                key.Binding
	Down              key.Binding
	Help              key.Binding
	Quit              key.Binding
	Switch            key.Binding
	Refresh           key.Binding
	AccountFilter     key.Binding
	DateFilter        key.Binding
	SwapSortingByDate key.Binding
	Search            key.Binding
	ResetFilters      key.Binding
	AcctDepth         key.Binding
}

// ShortHelp returns keybindings to be shown in the mini help view. It's part
// of the key.Map interface.
func (k keyMap) ShortHelp() []key.Binding {
	return []key.Binding{
		k.Help,
		k.Quit,
		k.Switch,
		k.Refresh,
		k.AccountFilter,
		k.DateFilter,
		k.SwapSortingByDate,
		k.Search,
		k.ResetFilters,
		k.AcctDepth,
	}
}

// FullHelp returns keybindings for the expanded help view. It's part of the
// key.Map interface.
func (k keyMap) FullHelp() [][]key.Binding {
	return [][]key.Binding{k.ShortHelp()}
}

var keys = keyMap{
	Up: key.NewBinding(
		key.WithKeys("up", "k"),
		key.WithHelp("↑/k", "move up"),
	),
	Down: key.NewBinding(
		key.WithKeys("down", "j"),
		key.WithHelp("↓/j", "move down"),
	),
	Help: key.NewBinding(
		key.WithKeys("?"),
		key.WithHelp("?", "toggle help"),
	),
	Quit: key.NewBinding(
		key.WithKeys("q", "esc", "ctrl+c"),
		key.WithHelp("q", "quit"),
	),
	Switch: key.NewBinding(
		key.WithKeys("left", "right"),
		key.WithHelp("←/→", "switch table"),
	),
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
	AcctDepth: key.NewBinding(
		key.WithKeys("-"),
		key.WithHelp("-", "reduce acct depth"),
	),
}

type helpModel struct {
	help       help.Model
	keys       keyMap
	inputStyle lipgloss.Style
	lastKey    string
	quitting   bool
}

func newHelpModel() helpModel {
	return helpModel{
		keys:       keys,
		help:       help.New(),
		inputStyle: lipgloss.NewStyle().Foreground(lipgloss.Color("#FF75B7")),
	}
}

func (m helpModel) Init() tea.Cmd {
	return nil
}

func (m helpModel) Update(msg tea.Msg) (tea.Model, tea.Cmd) {
	switch msg := msg.(type) {
	case tea.WindowSizeMsg:
		// If we set a width on the help menu it can it can gracefully truncate
		// its view as needed.
		m.help.Width = msg.Width
	}

	return m, nil
}

func (m helpModel) View() string {
	if m.quitting {
		return "Bye!\n"
	}

	style := lipgloss.NewStyle().MarginTop(1)
	return style.Render(m.help.View(m.keys))
}
