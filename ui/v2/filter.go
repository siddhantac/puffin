package ui

import (
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
	filters []*filter
	focused bool
}

func newFilterGroup() *filterGroup {
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
	// switch msg := msg.(type) {
	// case tea.KeyMsg:
	// 	switch msg.String() {
	// 	case "tab":
	// 		return fg, tea.Quit
	// 	}
	// }
	return fg, nil
}

func (fg *filterGroup) View() string {
	filterTitleStyle := lipgloss.NewStyle().
		Foreground(lipgloss.Color("#AAAAAA"))

	var view string
	for _, f := range fg.filters {
		filterView := lipgloss.JoinHorizontal(
			lipgloss.Top,
			filterTitleStyle.Render(f.name+":"),
			f.Model.View(),
		)
		view = lipgloss.JoinHorizontal(lipgloss.Left, view, filterView)
	}

	borderColor := "240"
	if fg.Focused() {
		borderColor = "White"
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
	fg.focused = true
}

func (fg *filterGroup) Blur() {
	fg.focused = false
}

func (fg *filterGroup) ViewAlt() string {
	filterTitleStyle := lipgloss.NewStyle().
		Foreground(lipgloss.Color("#AAAAAA"))

	accountFilter := lipgloss.JoinHorizontal(
		lipgloss.Top,
		filterTitleStyle.Render(account.name+":"),
		account.Model.View(),
	)
	return lipgloss.NewStyle().Border(lipgloss.RoundedBorder()).BorderForeground(lipgloss.Color("240")).Render(accountFilter)
}
