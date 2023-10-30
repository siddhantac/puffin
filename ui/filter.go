package ui

import (
	"puffin/hledger"
	"puffin/logger"

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

func newFilter() *filter {
	f := new(filter)
	f.account = textinput.New()
	f.account.Prompt = ""
	f.account.Placeholder = "-"

	f.date = textinput.New()
	f.date.Prompt = ""
	f.date.Placeholder = "-"
	return f
}

func (f *filter) Init() tea.Cmd {
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
			// f.Blur()
			return f, f.newFilter

			// TODO: implement tab system for filters too?
			// case key.Matches(msg, t.help.keys.Up):
			// case key.Matches(msg, t.help.keys.Down):
		case "down":
			logger.Logf("filter down")
			if f.account.Focused() {
				f.account.Blur()
				f.date.Focus()
				logger.Logf("date focused")
			}
		case "up":
			logger.Logf("filter up")
			if f.date.Focused() {
				f.date.Blur()
				f.account.Focus()
				logger.Logf("acc focused")
			}
		}
	}

	var cmd tea.Cmd
	var cmd2 tea.Cmd
	f.account, cmd = f.account.Update(msg)
	f.date, cmd2 = f.date.Update(msg)

	return f, tea.Batch(cmd, cmd2)
}

func (f *filter) newFilter() tea.Msg {
	if f.account.Focused() {
		logger.Logf("return account filter")
		return hledger.NewAccountFilter(
			f.account.Value(),
		)
	}
	if f.date.Focused() {
		logger.Logf("return date filter")
		return hledger.NewDateFilter().WithSmartDate(
			f.date.Value(),
		)
	}

	return nil
}

func (f *filter) Blur() {
	f.isFocused = false
	f.account.Blur()
	f.date.Blur()
}

func (f *filter) Focus() {
	f.isFocused = true
	f.account.Focus()
}

func (f *filter) IsFocused() bool {
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
		Render(f.account.View())

	dateFilter := filterTitle.Render("date")
	dateFilterData := lipgloss.NewStyle().
		MarginBottom(1).
		MarginRight(2).
		Render(f.date.View())

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
