package ui

import (
	"puffin/ui/colorscheme"

	"github.com/charmbracelet/bubbles/textinput"
	tea "github.com/charmbracelet/bubbletea"
	"github.com/charmbracelet/lipgloss"
)

type filterApplied struct{}

func (f *filterGroup) applyFilter() tea.Msg { return filterApplied{} }

type filter struct {
	textinput.Model
	name string
}

type filterGroup struct {
	account       *filter
	startDate     *filter
	endDate       *filter
	description   *filter
	isFocused     bool
	filters       []*filter
	focusedFilter int
}

func (f *filterGroup) setStartDate(startDate string) {
	f.startDate.Model.SetValue(startDate)
}

func (f *filterGroup) setEndDate(endDate string) {
	f.endDate.Model.SetValue(endDate)
}

func newFilterGroup() *filterGroup {
	f := new(filterGroup)

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

	f.description = &filter{
		Model: textinput.New(),
		name:  "description",
	}

	f.filters = []*filter{
		f.account,
		f.startDate,
		f.endDate,
		f.description,
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

func (f *filterGroup) Update(msg tea.Msg) tea.Cmd {
	// 'x' to reset
	switch msg := msg.(type) {
	case tea.KeyMsg:
		switch msg.String() {
		case "esc":
			f.Blur()
			return nil
		case "enter":
			return f.applyFilter

		// TODO: use proper key.Matches
		// case key.Matches(msg, f.keys.Down):
		case "down":
			f.filters[f.focusedFilter].Blur()
			f.focusedFilter++
			if f.focusedFilter >= len(f.filters) {
				f.focusedFilter = 0
			}
			return f.filters[f.focusedFilter].Focus()

		// case key.Matches(msg, f.keys.Up):
		case "up":
			f.filters[f.focusedFilter].Blur()
			f.focusedFilter--
			if f.focusedFilter < 0 {
				f.focusedFilter = len(f.filters) - 1
			}
			return f.filters[f.focusedFilter].Focus()
		}
	}

	fil := f.filters[f.focusedFilter]
	var cmd tea.Cmd
	fil.Model, cmd = fil.Update(msg)
	return cmd
}

func (f *filterGroup) Reset() {
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
	filterSectionTitleStyle := sectionTitleStyle.Copy()

	if f.isFocused {
		filterSectionTitleStyle.
			Background(lipgloss.Color(colorscheme.Nord0)).
			Bold(true)
	}
	filterSectionTitle := filterSectionTitleStyle.Render("FILTERS")

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

	sectionStyle := lipgloss.NewStyle().
		MarginRight(1).
		MarginLeft(1).
		BorderStyle(lipgloss.NormalBorder()).
		BorderForeground(theme.PrimaryForeground).
		BorderBottom(true)

	return sectionStyle.Render(
		lipgloss.JoinVertical(
			lipgloss.Right,
			filterList...,
		),
	)
}
