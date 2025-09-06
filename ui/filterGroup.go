package ui

import (
	"strings"
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
	// Compose fixed-alignment block matching the left pane style
	val := func(s string) string { if lipgloss.Width(strings.TrimSpace(s)) == 0 { return "-" }; return s }
	acc := val(f.account.View())
	sd := val(f.startDate.View())
	ed := val(f.endDate.View())
	desc := val(f.description.View())

	lines := []string{
		" \u001b[0m──────────────────", // literal separator; no styling
		"     " + sectionTitleStyle.Render("FILTERS"),
		"     account",
		"           " + acc,
		"",
		"  start date",
		"       " + sd,
		"",
		"    end date",
		"           " + ed,
		"",
		" description",
		"           " + desc,
		"",
		" [0m─────────────",
	}
	return strings.Join(lines, "\n")
}
