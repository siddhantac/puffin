package ui

import (
	"puffin/hledger"
	"puffin/ui/keys"
	"puffin/ui/tabs"

	"github.com/charmbracelet/bubbles/textinput"
	tea "github.com/charmbracelet/bubbletea"
	"github.com/charmbracelet/lipgloss"
)

type Refresh struct{}

type filterPanel struct {
	width         int
	dateQuery     textinput.Model
	accountQuery  textinput.Model
	keys          keys.KeyMap
	dateFilter    hledger.Filter
	accountFilter hledger.Filter
	filterTabs    *tabs.Tabs
	focused       bool
}

func newFilterPanel() *filterPanel {
	fp := &filterPanel{
		dateQuery:     textinput.New(),
		dateFilter:    hledger.NewDateFilter().UpToToday(),
		accountQuery:  textinput.New(),
		accountFilter: hledger.NoFilter{},
		keys:          keys.AllKeys,
		filterTabs:    tabs.New([]string{"date", "account"}),
		focused:       false,
	}
	fp.dateQuery.Prompt = "date: "
	fp.dateQuery.Placeholder = "'esc' to cancel"
	fp.accountQuery.Prompt = "account: "
	fp.accountQuery.Placeholder = "'esc' to cancel"
	return fp
}

func (f *filterPanel) IsFocused() bool {
	return f.focused
}

func (f *filterPanel) Focus() {
	f.focused = true
	f.dateQuery.Focus()
}

func (f *filterPanel) Filter() []hledger.Filter {
	return []hledger.Filter{f.dateFilter, f.accountFilter}
}

func (f *filterPanel) Init() tea.Cmd {
	return func() tea.Msg {
		return f.dateFilter
	}
}

func (f *filterPanel) Update(msg tea.Msg) (tea.Model, tea.Cmd) {
	// var cmd tea.Cmd
	// _, cmd = f.filterTabs.Update(msg)

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
				f.dateQuery.Blur()
				f.accountQuery.Blur()
				f.focused = false
				return f, func() tea.Msg { return Refresh{} }
			case "enter":
				f.dateQuery.Blur()
				f.accountQuery.Blur()
				f.dateFilter = hledger.NewDateFilter().WithSmartDate(f.dateQuery.Value())
				f.accountFilter = hledger.NewAccountFilter(f.accountQuery.Value())
				f.focused = false
				return f, func() tea.Msg { return Refresh{} }

			}
		}
	}

	if f.dateQuery.Focused() {
		var cmd tea.Cmd
		f.dateQuery, cmd = f.dateQuery.Update(msg)
		return f, cmd
	}

	if f.accountQuery.Focused() {
		var cmd tea.Cmd
		f.accountQuery, cmd = f.accountQuery.Update(msg)
		return f, cmd
	}

	return f, nil
}

func (f *filterPanel) View() string {

	return lipgloss.NewStyle().MarginBottom(1).Render(lipgloss.JoinVertical(lipgloss.Left,
		f.dateQuery.View(),
		f.accountQuery.View(),
		// styles.FilterPanelStyle.Render(lipgloss.NewStyle().PaddingRight(f.dateQuery.Width-len(f.dateQuery.Value())).Render(f.dateQuery.View())),
		// styles.FilterPanelStyle.Render(f.accountQuery.View()),
	))
}
