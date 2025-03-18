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

type filterGroup struct {
	account            *filter
	startDate, endDate *filter
}

func newFilterGroup() *filterGroup {
	return &filterGroup{
		account: &filter{
			Model: textinput.New(),
			name:  "account",
		},
		startDate: &filter{
			Model: textinput.New(),
			name:  "from",
		},
		endDate: &filter{
			Model: textinput.New(),
			name:  "to",
		},
	}
}

func (fg *filterGroup) Init() tea.Cmd {
	return nil
}

func (fg *filterGroup) Update(msg tea.Msg) (tea.Model, tea.Cmd) {
	return fg, nil
}

func (fg *filterGroup) View() string {
	filterTitleStyle := lipgloss.NewStyle().
		Foreground(lipgloss.Color("#AAAAAA"))

	accountFilter := lipgloss.JoinHorizontal(
		lipgloss.Top,
		filterTitleStyle.Render(fg.account.name+":"),
		fg.account.Model.View(),
	)
	return lipgloss.NewStyle().
		PaddingLeft(1).
		PaddingRight(1).
		Border(lipgloss.RoundedBorder()).
		BorderForeground(lipgloss.Color("240")).
		Render(accountFilter)
}

func (fg *filterGroup) ViewAlt() string {
	filterTitleStyle := lipgloss.NewStyle().
		Foreground(lipgloss.Color("#AAAAAA"))

	accountFilter := lipgloss.JoinHorizontal(
		lipgloss.Top,
		filterTitleStyle.Render(fg.account.name+":"),
		fg.account.Model.View(),
	)
	return lipgloss.NewStyle().Border(lipgloss.RoundedBorder()).BorderForeground(lipgloss.Color("240")).Render(accountFilter)
}
