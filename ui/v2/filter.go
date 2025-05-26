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

func accountFilter() *filter {
	ti := textinput.New()
	ti.Width = 40
	ti.CharLimit = 50
	ti.Prompt = ""
	return &filter{
		Model: ti,
		name:  "account",
	}
}

func startDate() *filter {
	ti := textinput.New()
	ti.Width = 12
	ti.CharLimit = 30
	ti.Prompt = ""

	return &filter{
		Model: ti,
		name:  "from",
	}
}

func endDate() *filter {
	ti := textinput.New()
	ti.Width = 12
	ti.CharLimit = 30
	ti.Prompt = ""
	return &filter{
		Model: ti,
		name:  "to",
	}
}

func description() *filter {
	ti := textinput.New()
	ti.Width = 30
	ti.CharLimit = 50
	ti.Prompt = ""
	return &filter{
		Model: ti,
		name:  "description",
	}
}

type filterGroup struct {
	account       *filter
	startDate     *filter
	endDate       *filter
	description   *filter
	filters       []*filter
	focused       bool
	focusedFilter int
}

func newFilterGroup() *filterGroup {

	fg := &filterGroup{
		account:     accountFilter(),
		startDate:   startDate(),
		endDate:     endDate(),
		description: description(),
	}

	return fg
}

func (fg *filterGroup) AccountName() string {
	return fg.account.Value()
}

func (fg *filterGroup) DateStart() string {
	return fg.startDate.Value()
}

func (fg *filterGroup) DateEnd() string {
	return fg.endDate.Value()
}

func (fg *filterGroup) Description() string {
	return fg.description.Value()
}

func (fg *filterGroup) Init() tea.Cmd {
	return nil
}

func (fg *filterGroup) Update(msg tea.Msg) (tea.Model, tea.Cmd) {
	switch msg := msg.(type) {
	case tea.WindowSizeMsg:
		allWidths := 0
		for i := 0; i < len(fg.filters)-1; i++ {
			allWidths = allWidths + fg.filters[i].GetWidth()
		}
		fg.description.Width = msg.Width - allWidths - 96
		return fg, nil

	case tea.KeyMsg:
		switch msg.String() {
		case "esc":
			return fg, blurFilterCmd
		case "enter":
			return fg, refreshDataCmd
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
	fg.focusedFilter = 0
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

type filterGroupFactory struct{}

func (f filterGroupFactory) NewGroupHome() *filterGroup {
	fg := newFilterGroup()
	fg.filters = []*filter{
		fg.startDate,
		fg.endDate,
		fg.account,
		fg.description,
	}
	return fg
}

func (f filterGroupFactory) NewGroupReports() *filterGroup {
	fg := newFilterGroup()
	fg.filters = []*filter{
		fg.startDate,
		fg.endDate,
		fg.account,
	}
	return fg
}

func (f filterGroupFactory) NewGroupBalance() *filterGroup {
	fg := newFilterGroup()
	fg.filters = []*filter{
		fg.startDate,
		fg.endDate,
		fg.account,
	}
	return fg
}
