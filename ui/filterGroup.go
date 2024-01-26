package ui

import (
	"puffin/accounting"
	"puffin/ui/colorscheme"

	"github.com/charmbracelet/bubbles/textinput"
	tea "github.com/charmbracelet/bubbletea"
	"github.com/charmbracelet/lipgloss"
)

type filterGroup struct {
	// acc filter
	account textinput.Model
	// date filter
	date textinput.Model
	// periodic filter
	// periodic textinput.Model // TODO: might make this a list of options?
	isFocused bool
	keys      keyMap
}

var defaultDateFilter = accounting.NewDateFilter().LastNYears(3)

func newFilterGroup() *filterGroup {
	f := new(filterGroup)
	f.keys = allKeys

	f.account = textinput.New()
	f.account.Prompt = ""
	f.account.Placeholder = "-"

	f.date = textinput.New()
	f.date.Prompt = ""
	f.date.Placeholder = "-"
	// f.date.SetValue(defaultDateFilter.Value())
	f.date.Blur()

	return f
}

func (f *filterGroup) DateFilter() accounting.Filter {
	return accounting.NewDateFilter().WithSmartDate(f.date.Value())
}

func (f *filterGroup) AccountFilter() accounting.Filter {
	return accounting.NewAccountFilter(f.account.Value())
}

func (f *filterGroup) Init() tea.Cmd {
	return nil
}

func (f *filterGroup) Update(msg tea.Msg) (tea.Model, tea.Cmd) {
	// 'x' to reset
	switch msg := msg.(type) {
	case tea.KeyMsg:
		switch msg.String() {
		case "q", "esc":
			f.Blur()
			return f, nil
		case "enter":
			return f, f.newFilter

		case "down":
			if f.account.Focused() {
				f.account.Blur()
				f.date.Focus()
			}
		case "up":
			if f.date.Focused() {
				f.date.Blur()
				f.account.Focus()
			}
		}
	}

	var cmd tea.Cmd
	var cmd2 tea.Cmd
	f.account, cmd = f.account.Update(msg)
	f.date, cmd2 = f.date.Update(msg)

	return f, tea.Batch(cmd, cmd2)
}

func (f *filterGroup) Reset() {
	f.account.Reset()
	f.date.SetValue(defaultDateFilter.Value())
	f.account.Blur()
	f.date.Blur()
}

func (f *filterGroup) newFilter() tea.Msg {
	if f.account.Focused() {
		return accounting.NewAccountFilter(
			f.account.Value(),
		)
	}
	if f.date.Focused() {
		return accounting.NewDateFilter().WithSmartDate(
			f.date.Value(),
		)
	}

	return nil
}

func (f *filterGroup) Blur() {
	f.isFocused = false
	f.account.Blur()
	f.date.Blur()
}

func (f *filterGroup) Focus() {
	f.isFocused = true
	f.account.Focus()
}

func (f *filterGroup) IsFocused() bool {
	return f.isFocused
}

func (f *filterGroup) View() string {
	filterStyle := lipgloss.NewStyle().
		MarginTop(1).
		MarginRight(1).
		PaddingRight(1).
		PaddingLeft(1).
		Foreground(theme.Accent)

	if f.isFocused {
		filterStyle.
			Background(lipgloss.Color(colorscheme.Nord0)).
			Bold(true)
	}
	filter := filterStyle.Render("FILTERS")

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

	// periodFilter := filterTitle.Render("periodic")
	// periodFilterData := lipgloss.NewStyle().
	// 	MarginBottom(1).
	// 	MarginRight(2).
	// 	Render("M / Y / Q") // TODO: connect to actual filters

	return lipgloss.JoinVertical(
		lipgloss.Right,
		filter,
		accFilter,
		accFilterData,
		dateFilter,
		dateFilterData,
		// periodFilter,
		// periodFilterData,
	)
}
