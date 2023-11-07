package ui

import (
	"fmt"
	"puffin/hledger"
	"puffin/ui/colorscheme"

	"github.com/charmbracelet/bubbles/textinput"
	tea "github.com/charmbracelet/bubbletea"
	"github.com/charmbracelet/lipgloss"
)

type filterGroup struct {
	account   textinput.Model
	date      textinput.Model
	period    focusable
	isFocused bool
	keys      keyMap
}

var defaultDateFilter = hledger.NewDateFilter().LastNMonths(6)

func newFilterGroup() *filterGroup {
	f := new(filterGroup)
	f.keys = allKeys

	f.account = textinput.New()
	f.account.Prompt = ""
	f.account.Placeholder = "-"

	f.date = textinput.New()
	f.date.Prompt = ""
	f.date.Placeholder = "-"
	f.date.SetValue(defaultDateFilter.Value())
	f.date.Blur()

	f.period = new(periodFilterFocusable)

	return f
}

func (f *filterGroup) DateFilter() hledger.Filter {
	return hledger.NewDateFilter().WithSmartDate(f.date.Value())
}

func (f *filterGroup) AccountFilter() hledger.Filter {
	return hledger.NewAccountFilter(f.account.Value())
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
			} else if f.date.Focused() {
				f.date.Blur()
				f.period.Focus()
			} else if f.period.Focused() {
				f.period.Blur()
				f.account.Focus()
			}
		case "up":
			if f.period.Focused() {
				f.period.Blur()
				f.date.Focus()
			} else if f.date.Focused() {
				f.date.Blur()
				f.account.Focus()
			} else if f.account.Focused() {
				f.account.Blur()
				f.period.Focus()
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
		return hledger.NewAccountFilter(
			f.account.Value(),
		)
	}
	if f.date.Focused() {
		return hledger.NewDateFilter().WithSmartDate(
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

	focusedFilterTitle := lipgloss.NewStyle().
		Foreground(theme.Accent).
		MarginRight(2)

	unfocusedFfilterTitle := lipgloss.NewStyle().
		Foreground(theme.PrimaryForeground).
		MarginRight(2)

	var accFilter string
	if f.account.Focused() {
		accFilter = focusedFilterTitle.Render("account")
	} else {
		accFilter = unfocusedFfilterTitle.Render("account")
	}

	accFilterData := lipgloss.NewStyle().
		MarginBottom(1).
		MarginRight(2).
		Render(f.account.View())

	var dateFilter string
	if f.date.Focused() {
		dateFilter = focusedFilterTitle.Render("date")
	} else {
		dateFilter = unfocusedFfilterTitle.Render("date")
	}
	dateFilterData := lipgloss.NewStyle().
		MarginBottom(1).
		MarginRight(2).
		Render(f.date.View())

	selectedPeriodStyle := lipgloss.NewStyle().Foreground(theme.Accent)
	monthly := "M"
	quarterly := "Q"
	yearly := "Y"
	sep := " / "
	m := selectedPeriodStyle.Render(monthly)

	var periodFilter string
	if f.period.Focused() {
		periodFilter = focusedFilterTitle.Render("periodic")
	} else {
		periodFilter = unfocusedFfilterTitle.Render("periodic")
	}
	periodFilterData := lipgloss.NewStyle().
		MarginBottom(1).
		MarginRight(2).
		Render(fmt.Sprintf("%s%s%s%s%s", m, sep, quarterly, sep, yearly)) // TODO: connect to actual filters

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
