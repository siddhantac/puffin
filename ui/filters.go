package ui

import (
	"puffin/hledger"

	"github.com/charmbracelet/bubbles/key"
	"github.com/charmbracelet/bubbles/textinput"
	tea "github.com/charmbracelet/bubbletea"
)

type Refresh struct{}

type filterPanel struct {
	dateQuery  textinput.Model
	help       helpModel
	value      string
	dateFilter hledger.DateFilter
}

func newFilterPanel() *filterPanel {
	fp := &filterPanel{
		dateQuery:  textinput.New(),
		help:       newHelpModel(),
		dateFilter: hledger.NewDateFilter().UpToToday(),
	}
	fp.dateQuery.Placeholder = "date filter ('esc' to cancel)"
	return fp
}

func (f *filterPanel) Filter() hledger.Filter {
	return f.dateFilter
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
			return f, f.dateQuery.Focus()
		default:
			switch msg.String() {
			case "esc", "q", "ctrl+c":
				f.dateQuery.Blur()
				return f, nil
			case "enter":
				f.dateQuery.Blur()
				f.dateFilter = hledger.NewDateFilter().WithSmartDate(f.dateQuery.Value())
				return f, func() tea.Msg { return Refresh{} }

			}
		}
	}

	if f.dateQuery.Focused() {
		var cmd tea.Cmd
		f.dateQuery, cmd = f.dateQuery.Update(msg)
		return f, cmd
	}

	return f, nil
}

func (f *filterPanel) View() string {
	return filterPanelStyle.Render(f.dateQuery.View())
}

func (f *filterPanel) Value() tea.Msg {
	return hledger.NewDateFilter().WithSmartDate(f.dateQuery.Value())
}
