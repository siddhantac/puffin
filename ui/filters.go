package ui

import (
	"puffin/hledger"
	"puffin/ui/styles"

	"github.com/charmbracelet/bubbles/key"
	"github.com/charmbracelet/bubbles/textinput"
	tea "github.com/charmbracelet/bubbletea"
	"github.com/charmbracelet/lipgloss"
)

type Refresh struct{}

type filterPanel struct {
	dateQuery     textinput.Model
	accountQuery  textinput.Model
	help          helpModel
	dateFilter    hledger.Filter
	accountFilter hledger.Filter
}

func newFilterPanel() *filterPanel {
	fp := &filterPanel{
		dateQuery:     textinput.New(),
		dateFilter:    hledger.NewDateFilter().UpToToday(),
		accountQuery:  textinput.New(),
		accountFilter: hledger.NoFilter{},
		help:          newHelpModel(),
	}
	fp.dateQuery.Placeholder = "date filter ('esc' to cancel)"
	fp.accountQuery.Placeholder = "account filter ('esc' to cancel)"
	return fp
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
	switch msg := msg.(type) {
	case tea.KeyMsg:
		switch {
		case key.Matches(msg, f.help.keys.AccountFilter):
			return f, f.accountQuery.Focus()
		case key.Matches(msg, f.help.keys.DateFilter):
			return f, f.dateQuery.Focus()
		default:
			switch msg.String() {
			case "esc", "q", "ctrl+c":
				f.dateQuery.Blur()
				f.accountQuery.Blur()
				return f, nil
			case "enter":
				f.dateQuery.Blur()
				f.accountQuery.Blur()
				f.dateFilter = hledger.NewDateFilter().WithSmartDate(f.dateQuery.Value())
				f.accountFilter = hledger.NewAccountFilter(f.accountQuery.Value())
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
	return lipgloss.JoinHorizontal(lipgloss.Center,
		styles.FilterPanelStyle.Render(f.dateQuery.View()),
		styles.FilterPanelStyle.Render(f.accountQuery.View()),
	)
}
