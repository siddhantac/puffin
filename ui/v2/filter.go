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

func (f *filter) GetWidth() int {
	return len(f.name) + f.Model.Width
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

var description = &filter{
	Model: textinput.New(),
	name:  "description",
}

type filterGroup struct {
	filters       []*filter
	focused       bool
	focusedFilter int
}

func newFilterGroup() *filterGroup {
	account.Width = 40
	account.CharLimit = 50
	account.Prompt = ""

	startDate.Width = 12
	startDate.CharLimit = 30
	startDate.Prompt = ""

	endDate.Width = 12
	endDate.CharLimit = 30
	endDate.Prompt = ""

	description.Width = 30
	description.CharLimit = 50
	description.Prompt = ""

	return &filterGroup{
		filters: []*filter{
			startDate,
			endDate,
			account,
			description,
		},
	}
}

func (fg *filterGroup) Init() tea.Cmd {
	return nil
}

func (fg *filterGroup) Update(msg tea.Msg) (tea.Model, tea.Cmd) {
	switch msg := msg.(type) {
	case tea.WindowSizeMsg:
		description.Width = msg.Width - startDate.GetWidth() - endDate.GetWidth() - account.Width - 46
		return fg, nil

	case tea.KeyMsg:
		switch msg.String() {
		case "enter":
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

	var view string
	for _, f := range fg.filters {
		filterView := lipgloss.NewStyle().
			Foreground(lipgloss.Color("White")).
			Render(f.name + ": " + f.Model.View())

		borderColor := unfocusedColor
		if f.Focused() {
			borderColor = focusedColor
		}
		view = lipgloss.JoinHorizontal(lipgloss.Left,
			view,
			lipgloss.NewStyle().
				PaddingLeft(1).
				PaddingRight(1).
				Border(lipgloss.RoundedBorder()).
				BorderForeground(lipgloss.Color(borderColor)).
				Render(filterView),
		)
	}
	return view
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
	fg.filters[fg.focusedFilter].Blur()
}

func (fg *filterGroup) focusNext() {
	log.Printf("focused filter: %d", fg.focusedFilter)
	fg.filters[fg.focusedFilter].Blur()
	fg.focusedFilter++
	if fg.focusedFilter >= len(fg.filters) {
		fg.focusedFilter = 0
	}
	fg.filters[fg.focusedFilter].Focus()
	log.Printf("focus next filter: %d", fg.focusedFilter)
}

func (fg *filterGroup) focusPrev() {
	fg.filters[fg.focusedFilter].Blur()
	fg.focusedFilter--
	if fg.focusedFilter < 0 {
		fg.focusedFilter = len(fg.filters) - 1
	}
	fg.filters[fg.focusedFilter].Focus()
	log.Printf("focus prev filter: %d", fg.focusedFilter)
}
