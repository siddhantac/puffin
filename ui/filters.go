package ui

import (
	"puffin/hledger"
	"puffin/ui/keys"
	"puffin/ui/tabs"

	"github.com/charmbracelet/bubbles/textinput"
	tea "github.com/charmbracelet/bubbletea"
	"github.com/charmbracelet/lipgloss"
)

type filter struct {
	textinput.Model
	hlfilter hledger.Filter
}

type Refresh struct{}

type filterPanel struct {
	width         int
	keys          keys.KeyMap
	dateFilter    filter
	accountFilter filter
	filterTabs    *tabs.Tabs
	focused       bool
}

func newFilterPanel() *filterPanel {
	fp := &filterPanel{
		dateFilter: filter{
			Model:    textinput.New(),
			hlfilter: hledger.NewDateFilter().UpToToday(),
		},
		accountFilter: filter{
			Model:    textinput.New(),
			hlfilter: hledger.NoFilter{},
		},
		keys:       keys.AllKeys,
		filterTabs: tabs.New([]string{"date", "account"}),
		focused:    false,
	}
	fp.dateFilter.Prompt = "date: "
	fp.dateFilter.Placeholder = "'esc' to cancel"
	fp.accountFilter.Prompt = "account: "
	fp.accountFilter.Placeholder = "'esc' to cancel"
	return fp
}

func (f *filterPanel) IsFocused() bool {
	return f.focused
}

func (f *filterPanel) Focus() {
	f.focused = true
	f.dateFilter.Focus()
}

func (f *filterPanel) Filters() []hledger.Filter {
	return []hledger.Filter{f.dateFilter.hlfilter, f.accountFilter.hlfilter}
}

func (f *filterPanel) Init() tea.Cmd {
	return func() tea.Msg {
		return f.dateFilter
	}
}

func (f *filterPanel) Update(msg tea.Msg) (tea.Model, tea.Cmd) {
	switch msg := msg.(type) {
	case tea.KeyMsg:
		switch {
		// TODO: switch focus on filters when right-left pressed
		// case key.Matches(msg, f.help.keys.Left):
		// 	return f, f.accountQuery.Focus()
		// case key.Matches(msg, f.help.keys.Right):
		// 	return f, f.dateQuery.Focus()
		default:
			switch msg.String() {
			case "esc", "q", "ctrl+c":
				f.dateFilter.Blur()
				f.accountFilter.Blur()
				f.focused = false
				return f, func() tea.Msg { return Refresh{} }
			case "enter":
				f.dateFilter.Blur()
				f.accountFilter.Blur()
				f.dateFilter.hlfilter = hledger.NewDateFilter().WithSmartDate(f.dateFilter.Value())
				f.accountFilter.hlfilter = hledger.NewAccountFilter(f.accountFilter.Value())
				f.focused = false
				return f, func() tea.Msg { return Refresh{} }

			}
		}
	}

	if f.dateFilter.Focused() {
		var cmd tea.Cmd
		f.dateFilter.Model, cmd = f.dateFilter.Update(msg)
		return f, cmd
	}

	if f.accountFilter.Focused() {
		var cmd tea.Cmd
		f.accountFilter.Model, cmd = f.accountFilter.Update(msg)
		return f, cmd
	}

	return f, nil
}

func (f *filterPanel) View() string {

	return lipgloss.NewStyle().MarginBottom(1).Render(lipgloss.JoinVertical(lipgloss.Left,
		f.dateFilter.View(),
		f.accountFilter.View(),
		// styles.FilterPanelStyle.Render(lipgloss.NewStyle().PaddingRight(f.dateQuery.Width-len(f.dateQuery.Value())).Render(f.dateQuery.View())),
		// styles.FilterPanelStyle.Render(f.accountQuery.View()),
	))
}
