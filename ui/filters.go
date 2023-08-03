package ui

import (
	"puffin/hledger"

	"github.com/charmbracelet/bubbles/key"
	"github.com/charmbracelet/bubbles/textinput"
	tea "github.com/charmbracelet/bubbletea"
)

type Refresh struct{}

type filterPanel struct {
	query  textinput.Model
	help   helpModel
	value  string
	filter hledger.DateFilter
}

func newFilterPanel() *filterPanel {
	fp := &filterPanel{
		query:  textinput.New(),
		help:   newHelpModel(),
		filter: hledger.NewDateFilter().UpToToday(),
	}
	fp.query.Placeholder = "date filter ('esc' to cancel)"
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
			return f, f.query.Focus()
		default:
			switch msg.String() {
			case "esc", "q", "ctrl+c":
				f.query.Blur()
				return f, nil
			case "enter":
				f.query.Blur()
				f.filter = hledger.NewDateFilter().WithSmartDate(f.query.Value())
				return f, func() tea.Msg { return Refresh{} }

			}
		}
	}

	if f.query.Focused() {
		var cmd tea.Cmd
		f.query, cmd = f.query.Update(msg)
		return f, cmd
	}

	return f, nil
}

func (f *filterPanel) View() string {
	return filterPanelStyle.Render(f.query.View())
}

func (f *filterPanel) Value() tea.Msg {
	return hledger.NewDateFilter().WithSmartDate(f.query.Value())
}
