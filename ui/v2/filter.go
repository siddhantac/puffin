package ui

import (
	"log"

	"github.com/charmbracelet/bubbles/textinput"
	tea "github.com/charmbracelet/bubbletea"
	"github.com/charmbracelet/lipgloss"
)

type filter struct {
	textinput.Model
	name string
}

var account = &filter{
	Model: textinput.New(),
	name:  "account",
}
var startDate = &filter{
	Model: textinput.New(),
	name:  "from",
}

var endDate = &filter{
	Model: textinput.New(),
	name:  "to",
}

type filterGroup struct {
	filters       []*filter
	focused       bool
	focusedFilter int
}

func newFilterGroup() *filterGroup {
	account.Width = 50
	account.CharLimit = 100

	startDate.Width = 12
	startDate.CharLimit = 30
	endDate.Width = 12
	endDate.CharLimit = 30

	return &filterGroup{
		filters: []*filter{
			account,
			startDate,
			endDate,
		},
	}
}

func (fg *filterGroup) Init() tea.Cmd {
	return nil
}

func (fg *filterGroup) Update(msg tea.Msg) (tea.Model, tea.Cmd) {
	switch msg := msg.(type) {
	case tea.KeyMsg:
		switch msg.String() {
		case "esc":
			fg.Blur()
			return fg, nil
		case "tab":
			fg.focusNext()
			return fg, nil
		case "shift+tab":
			fg.focusPrev()
			return fg, nil
		}
	}
	fil := fg.filters[fg.focusedFilter]
	var cmd tea.Cmd
	fil.Model, cmd = fil.Update(msg)
	return fg, cmd
}

func (fg *filterGroup) View() string {
	focusedColor := "White"
	unfocusedColor := "240"

	focusedFilterTitle := lipgloss.NewStyle().
		Foreground(lipgloss.Color(focusedColor))
	unfocusedFilterTitle := lipgloss.NewStyle().
		Foreground(lipgloss.Color(unfocusedColor))

	var view string
	for _, f := range fg.filters {
		var filterView string
		if f.Focused() {
			filterView = focusedFilterTitle.Render(f.name + ":")
		} else {
			filterView = unfocusedFilterTitle.Render(f.name + ":")
		}

		view = lipgloss.JoinHorizontal(lipgloss.Left,
			view,
			lipgloss.JoinHorizontal(
				lipgloss.Top,
				filterView,
				f.Model.View(),
			))
	}

	borderColor := unfocusedColor
	if fg.Focused() {
		borderColor = focusedColor
	}

	return lipgloss.NewStyle().
		PaddingLeft(1).
		PaddingRight(1).
		Border(lipgloss.RoundedBorder()).
		BorderForeground(lipgloss.Color(borderColor)).
		Render(view)
}

func (fg *filterGroup) Focused() bool {
	return fg.focused
}

func (fg *filterGroup) Focus() {
	fg.filters[0].Focus()
	fg.focused = true
}

func (fg *filterGroup) Blur() {
	fg.focused = false
}

func (fg *filterGroup) focusNext() {
	log.Printf("focused filter: %d", fg.focusedFilter)
	fg.filters[fg.focusedFilter].Blur()
	fg.focusedFilter++
	log.Printf("focused filter: %d", fg.focusedFilter)
	if fg.focusedFilter >= len(fg.filters) {
		fg.focusedFilter = 0
	}
	fg.filters[fg.focusedFilter].Focus()
	log.Printf("focused filter: %d", fg.focusedFilter)
}

func (fg *filterGroup) focusPrev() {
	fg.filters[fg.focusedFilter].Blur()
	fg.focusedFilter--
	if fg.focusedFilter < 0 {
		fg.focusedFilter = len(fg.filters) - 1
	}
	fg.filters[fg.focusedFilter].Focus()
}
