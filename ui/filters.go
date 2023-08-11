package ui

import (
	"puffin/hledger"
	"puffin/ui/keys"
	"puffin/ui/styles"

	"github.com/charmbracelet/bubbles/key"
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
	width             int
	keys              keys.KeyMap
	dateFilter        filter
	accountFilter     filter
	descriptionFilter filter
	focused           bool
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
		descriptionFilter: filter{
			Model:    textinput.New(),
			hlfilter: hledger.NoFilter{},
		},
		keys:    keys.AllKeys,
		focused: false,
	}

	fp.dateFilter.Prompt = "date: "
	fp.dateFilter.Placeholder = "'esc' to cancel"

	fp.accountFilter.Prompt = "account: "
	fp.accountFilter.Placeholder = "'esc' to cancel"

	fp.descriptionFilter.Prompt = "description: "
	fp.descriptionFilter.Placeholder = "'esc' to cancel"

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
	return []hledger.Filter{
		f.dateFilter.hlfilter,
		f.accountFilter.hlfilter,
		f.descriptionFilter.hlfilter,
	}
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
		case key.Matches(msg, f.keys.Down):
			if f.dateFilter.Focused() {
				f.dateFilter.Blur()
				f.accountFilter.Focus()
				return f, nil
			}
			if f.accountFilter.Focused() {
				f.dateFilter.Blur()
				f.accountFilter.Blur()
				f.descriptionFilter.Focus()
				return f, nil
			}
			if f.descriptionFilter.Focused() {
				f.dateFilter.Focus()
				f.accountFilter.Blur()
				f.descriptionFilter.Blur()
				return f, nil
			}

		default:
			switch msg.String() {
			case "esc", "q", "ctrl+c":
				f.dateFilter.Blur()
				f.accountFilter.Blur()
				f.descriptionFilter.Blur()
				f.focused = false
				return f, func() tea.Msg { return Refresh{} }
			case "enter":
				f.dateFilter.Blur()
				f.accountFilter.Blur()
				f.descriptionFilter.Blur()
				f.dateFilter.hlfilter = hledger.NewDateFilter().WithSmartDate(f.dateFilter.Value())
				f.accountFilter.hlfilter = hledger.NewAccountFilter(f.accountFilter.Value())
				f.descriptionFilter.hlfilter = hledger.NewDescriptionFilter(f.descriptionFilter.Value())
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

	if f.descriptionFilter.Focused() {
		var cmd tea.Cmd
		f.descriptionFilter.Model, cmd = f.descriptionFilter.Update(msg)
		return f, cmd
	}

	return f, nil
}

func (f *filterPanel) View() string {
	return styles.FilterPanelStyle.Render(lipgloss.JoinVertical(lipgloss.Left,
		f.dateFilter.View(),
		f.accountFilter.View(),
		f.descriptionFilter.View(),
		// styles.FilterPanelStyle.Render(lipgloss.NewStyle().PaddingRight(f.dateQuery.Width-len(f.dateQuery.Value())).Render(f.dateQuery.View())),
		// styles.FilterPanelStyle.Render(f.accountQuery.View()),
	))

}
