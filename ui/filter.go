package ui

import (
	"puffin/hledger"

	"github.com/charmbracelet/bubbles/textinput"
	tea "github.com/charmbracelet/bubbletea"
	"github.com/charmbracelet/lipgloss"
)

type filter struct {
	// acc filter
	account textinput.Model
	// date filter
	date textinput.Model
	// periodic filter
	// periodic textinput.Model // TODO: might make this a list of options?
    isFocused bool
}

func (f *filter) Init() tea.Cmd {
	f.account = textinput.New()
	f.account.Placeholder = "account"
	return nil
}

func (f *filter) Update(msg tea.Msg) (tea.Model, tea.Cmd) {
	// keybinds in main model NOT HERE
	// 'up/down' to navigate
	// 'enter' to apply
	// 'x' to reset
	switch msg := msg.(type) {
	case tea.KeyMsg:
		switch msg.String() {
		case "esc":
			f.Blur()
			return f, nil
		case "enter":
			f.Blur()
			return f, f.newFilter

        // TODO: implement tab system for filters too?
        // case key.Matches(msg, t.help.keys.Up):
        // case key.Matches(msg, t.help.keys.Down):
		}
	}

	var cmd tea.Cmd
	f.account, cmd = f.account.Update(msg)

	return f, cmd
}

func (f *filter) newFilter() tea.Msg {
	return hledger.NewAccountFilter(f.account.Value())
}

func (f *filter) Blur() {
    f.isFocused = false
	f.account.Blur()
}

func (f *filter) Focus() {
    f.isFocused = true
	f.account.Focus()
}

func (f *filter) IsFocused()  bool {
    return f.isFocused
}

func (f *filter) View() string {
	filter := lipgloss.NewStyle().
		MarginTop(1).
		MarginRight(2).
		Foreground(theme.Accent).
		Render("FILTERS")

	filterTitle := lipgloss.NewStyle().
		Foreground(theme.PrimaryForeground).
		MarginRight(2)

	accFilter := filterTitle.Render("account")
	accFilterData := lipgloss.NewStyle().
		MarginBottom(1).
		MarginRight(2).
		Render(f.account.View()) // TODO: connect to actual filters

	dateFilter := filterTitle.Render("date")
	dateFilterData := lipgloss.NewStyle().
		MarginBottom(1).
		MarginRight(2).
		Render("2023") // TODO: connect to actual filters

	periodFilter := filterTitle.Render("periodic")
	periodFilterData := lipgloss.NewStyle().
		MarginBottom(1).
		MarginRight(2).
		Render("M / Y / Q") // TODO: connect to actual filters

	return lipgloss.JoinVertical(
		lipgloss.Right,
		filter,
		accFilter,
		accFilterData,
		dateFilter,
		dateFilterData,
		periodFilter,
		periodFilterData,
	)
}
