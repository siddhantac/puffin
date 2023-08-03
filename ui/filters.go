package ui

import (
	"puffin/hledger"

	"github.com/charmbracelet/bubbles/key"
	"github.com/charmbracelet/bubbles/textinput"
	tea "github.com/charmbracelet/bubbletea"
)

type Refresh struct{}

type filterPanel struct {
	dateFilter textinput.Model
	help       helpModel
	value      string
	filter     hledger.DateFilter
}

func newFilterPanel() *filterPanel {
	fp := &filterPanel{
		dateFilter: textinput.New(),
		help:       newHelpModel(),
		filter:     hledger.NewDateFilter().UpToToday(),
	}
	fp.dateFilter.Placeholder = "date filter ('esc' to cancel)"
	return fp
}

func (f *filterPanel) Filter() hledger.Filter {
	return f.filter
}

func (f *filterPanel) Init() tea.Cmd {
	return func() tea.Msg {
		return hledger.NewDateFilter().LastMonth()
	}
}

func (f *filterPanel) Update(msg tea.Msg) (tea.Model, tea.Cmd) {
	switch msg := msg.(type) {
	case tea.KeyMsg:
		switch {
		case key.Matches(msg, f.help.keys.DateFilter):
			return f, f.dateFilter.Focus()
		default:
			switch msg.String() {
			case "esc", "q", "ctrl+c":
				f.dateFilter.Blur()
				return f, nil
			case "enter":
				f.dateFilter.Blur()
				f.filter = hledger.NewDateFilter().WithSmartDate(f.dateFilter.Value())
				return f, func() tea.Msg { return Refresh{} }

			}
		}
	}

	if f.dateFilter.Focused() {
		var cmd tea.Cmd
		f.dateFilter, cmd = f.dateFilter.Update(msg)
		return f, cmd
	}

	return f, nil
}

func (f *filterPanel) View() string {
	return filterPanelStyle.Render(f.dateFilter.View())
}

func (f *filterPanel) Value() tea.Msg {
	return hledger.NewDateFilter().WithSmartDate(f.dateFilter.Value())
}
