package ui

import (
	tea "github.com/charmbracelet/bubbletea"
	"github.com/charmbracelet/lipgloss"
)

type Period struct{}

func (p *Period) Init() tea.Cmd {
	return nil
}

func (p *Period) Update(msg tea.Msg) (tea.Model, tea.Cmd) {
	switch msg := msg.(type) {
	case tea.KeyMsg:
		switch msg.String() {
		case "q", "esc":
			return p, tea.Quit
		}
	}
	return p, nil
}

func (p *Period) View() string {
	sectionTitleStyle := lipgloss.NewStyle().
		MarginTop(1).
		MarginRight(1).
		PaddingRight(1).
		PaddingLeft(1).
		Foreground(theme.Accent)
	sectionTitle := sectionTitleStyle.Render("PERIOD")

	inactiveTextStyle := lipgloss.NewStyle().
		Foreground(theme.PrimaryForeground).
		MarginRight(2)
	textStyle := lipgloss.NewStyle().
		MarginRight(2)

	return lipgloss.JoinVertical(
		lipgloss.Right,
		sectionTitle,
		inactiveTextStyle.Render("monthly"),
		inactiveTextStyle.Render("quarterly"),
		textStyle.Render("yearly"),
	)
}
