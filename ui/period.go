package ui

import (
	tea "github.com/charmbracelet/bubbletea"
	"github.com/charmbracelet/lipgloss"
)

type periodType string

const (
	monthly periodType = "monthly"
	quarter periodType = "quarterly"
	yearly  periodType = "yearly"
)

type Period struct {
	periodType periodType
}

func newPeriod() *Period {
	return &Period{periodType: yearly}
}

func (p *Period) Init() tea.Cmd {
	p.periodType = yearly
	return nil
}

func (p *Period) Update(msg tea.Msg) (tea.Model, tea.Cmd) {
	switch msg := msg.(type) {
	case tea.KeyMsg:
		switch msg.String() {
		case "m":
			p.periodType = monthly
		case "u":
			p.periodType = quarter
		case "y":
			p.periodType = yearly
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

	var monthView, quarterView, yearView string
	switch p.periodType {
	case monthly:
		monthView = textStyle.Render("monthly")
		quarterView = inactiveTextStyle.Render("quarterly")
		yearView = inactiveTextStyle.Render("yearly")
	case quarter:
		monthView = inactiveTextStyle.Render("monthly")
		quarterView = textStyle.Render("quarterly")
		yearView = inactiveTextStyle.Render("yearly")
	case yearly:
		monthView = inactiveTextStyle.Render("monthly")
		quarterView = inactiveTextStyle.Render("quarterly")
		yearView = textStyle.Render("yearly")
	}

	return lipgloss.JoinVertical(
		lipgloss.Right,
		sectionTitle,
		monthView,
		quarterView,
		yearView,
	)
}
