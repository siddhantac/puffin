package ui

import (
	"puffin/accounting"
	"puffin/ui/colorscheme"

	"github.com/charmbracelet/bubbles/textinput"
	tea "github.com/charmbracelet/bubbletea"
	"github.com/charmbracelet/lipgloss"
)

type filter struct {
	textinput.Model
	name string
}

type filterGroup struct {
	account   *filter
	startDate *filter
	endDate   *filter
	// periodic filter
	// periodic textinput.Model // TODO: might make this a list of options?
	isFocused     bool
	keys          keyMap
	filters       []*filter
	focusedFilter int
}

var defaultDateFilter = accounting.NewDateFilter().LastNYears(3)

func newFilterGroup() *filterGroup {
	f := new(filterGroup)
	f.keys = allKeys

	f.account = &filter{
		Model: textinput.New(),
		name:  "account",
	}

	f.startDate = &filter{
		Model: textinput.New(),
		name:  "start date",
	}

	f.endDate = &filter{
		Model: textinput.New(),
		name:  "end date",
	}

	f.filters = []*filter{
		f.account,
		f.startDate,
		f.endDate,
	}

	for _, fil := range f.filters {
		fil.Prompt = ""
		fil.Placeholder = "-"
		fil.Blur()
	}

	return f
}

func (f *filterGroup) Init() tea.Cmd {
	return nil
}

func dummy() tea.Msg {
	return accounting.NoFilter{}
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
			return f, dummy

		// TODO: use proper key.Matches
		// case key.Matches(msg, f.keys.Down):
		case "down":
			f.filters[f.focusedFilter].Blur()
			f.focusedFilter++
			if f.focusedFilter >= len(f.filters) {
				f.focusedFilter = 0
			}
			return f, f.filters[f.focusedFilter].Focus()

		// case key.Matches(msg, f.keys.Up):
		case "up":
			f.filters[f.focusedFilter].Blur()
			f.focusedFilter--
			if f.focusedFilter < 0 {
				f.focusedFilter = len(f.filters) - 1
			}
			return f, f.filters[f.focusedFilter].Focus()
		}
	}

	fil := f.filters[f.focusedFilter]
	var cmd tea.Cmd
	fil.Model, cmd = fil.Update(msg)
	return f, cmd
}

func (f *filterGroup) Reset() {
	f.account.Reset()
	f.startDate.SetValue(defaultDateFilter.Value())
	for _, m := range f.filters {
		m.Reset()
		m.Blur()
	}
}

func (f *filterGroup) Blur() {
	f.isFocused = false
	for _, fil := range f.filters {
		fil.Blur()
	}
}

func (f *filterGroup) Focus() {
	f.isFocused = true
	f.filters[0].Focus()
}

func (f *filterGroup) IsFocused() bool {
	return f.isFocused
}

func (f *filterGroup) View() string {
	filterSectionStyle := lipgloss.NewStyle().
		MarginTop(1).
		MarginRight(1).
		PaddingRight(1).
		PaddingLeft(1).
		Foreground(theme.Accent)

	if f.isFocused {
		filterSectionStyle.
			Background(lipgloss.Color(colorscheme.Nord0)).
			Bold(true)
	}
	filterSectionTitle := filterSectionStyle.Render("FILTERS")

	filterTitleStyle := lipgloss.NewStyle().
		Foreground(theme.PrimaryForeground).
		MarginRight(2)

	filterList := make([]string, 0, len(f.filters)*2+1)
	filterList = append(filterList, filterSectionTitle)

	for _, fil := range f.filters {
		filterTitle := filterTitleStyle.Render(fil.name)
		filterData := lipgloss.NewStyle().
			MarginBottom(1).
			MarginRight(2).
			Render(fil.View())
		filterList = append(filterList, filterTitle, filterData)
	}

	return lipgloss.JoinVertical(
		lipgloss.Right,
		filterList...,
	)
}
