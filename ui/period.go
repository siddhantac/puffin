package ui

import (
	tea "github.com/charmbracelet/bubbletea"
	"github.com/charmbracelet/lipgloss"
	"github.com/siddhantac/hledger"
)

type Period struct {
	periodType hledger.PeriodType
}

func newPeriod(period hledger.PeriodType) *Period {
	return &Period{periodType: period}
}

func (p *Period) Update(msg tea.Msg) {
	switch msg := msg.(type) {
	case tea.KeyMsg:
		switch msg.String() {
		case "w":
			p.periodType = hledger.PeriodWeekly
		case "m":
			p.periodType = hledger.PeriodMonthly
		case "u":
			p.periodType = hledger.PeriodQuarterly
		case "y":
			p.periodType = hledger.PeriodYearly
		}
	}
}

func (p *Period) View() string {
	periodTitleStyle := sectionTitleStyle.
		Copy().
		Render("PERIOD")

	inactiveTextStyle := lipgloss.NewStyle().
		Foreground(theme.PrimaryForeground).
		MarginRight(2)
	activeTextStyle := lipgloss.NewStyle().
		MarginRight(2)

	var (
		weekView    = inactiveTextStyle.Render("weekly")
		monthView   = inactiveTextStyle.Render("monthly")
		quarterView = inactiveTextStyle.Render("quarterly")
		yearView    = inactiveTextStyle.Render("yearly")
	)

	switch p.periodType {
	case hledger.PeriodWeekly:
		weekView = activeTextStyle.Render("weekly")
	case hledger.PeriodMonthly:
		monthView = activeTextStyle.Render("monthly")
	case hledger.PeriodQuarterly:
		quarterView = activeTextStyle.Render("quarterly")
	case hledger.PeriodYearly:
		yearView = activeTextStyle.Render("yearly")
	}

	return lipgloss.JoinVertical(
		lipgloss.Right,
		periodTitleStyle,
		weekView,
		monthView,
		quarterView,
		yearView,
	)
}
