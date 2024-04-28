package ui

import (
	tea "github.com/charmbracelet/bubbletea"
	"github.com/charmbracelet/lipgloss"
	"github.com/siddhantac/hledger"
)

type Period struct {
	periodType hledger.PeriodType
}

func newPeriod() *Period {
	return &Period{periodType: hledger.PeriodYearly}
}

func (p *Period) Init() tea.Cmd { return nil }

func (p *Period) Update(msg tea.Msg) (tea.Model, tea.Cmd) {
	switch msg := msg.(type) {
	case tea.KeyMsg:
		switch msg.String() {
		case "m":
			p.periodType = hledger.PeriodMonthly
		case "u":
			p.periodType = hledger.PeriodQuarterly
		case "y":
			p.periodType = hledger.PeriodYearly
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
	case hledger.PeriodMonthly:
		monthView = textStyle.Render("monthly")
		quarterView = inactiveTextStyle.Render("quarterly")
		yearView = inactiveTextStyle.Render("yearly")
	case hledger.PeriodQuarterly:
		monthView = inactiveTextStyle.Render("monthly")
		quarterView = textStyle.Render("quarterly")
		yearView = inactiveTextStyle.Render("yearly")
	case hledger.PeriodYearly:
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
