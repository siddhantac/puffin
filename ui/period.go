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
	periodTitleStyle := sectionTitleStyle.Copy()
	sectionTitle := periodTitleStyle.Render("PERIOD")

	inactiveTextStyle := lipgloss.NewStyle().
		Foreground(theme.PrimaryForeground).
		MarginRight(2)
	textStyle := lipgloss.NewStyle().
		MarginRight(2)

	var weekView, monthView, quarterView, yearView string
	switch p.periodType {
	case hledger.PeriodWeekly:
		weekView = textStyle.Render("weekly")
		monthView = inactiveTextStyle.Render("monthly")
		quarterView = inactiveTextStyle.Render("quarterly")
		yearView = inactiveTextStyle.Render("yearly")
	case hledger.PeriodMonthly:
		weekView = inactiveTextStyle.Render("weekly")
		monthView = textStyle.Render("monthly")
		quarterView = inactiveTextStyle.Render("quarterly")
		yearView = inactiveTextStyle.Render("yearly")
	case hledger.PeriodQuarterly:
		weekView = inactiveTextStyle.Render("weekly")
		monthView = inactiveTextStyle.Render("monthly")
		quarterView = textStyle.Render("quarterly")
		yearView = inactiveTextStyle.Render("yearly")
	case hledger.PeriodYearly:
		weekView = inactiveTextStyle.Render("weekly")
		monthView = inactiveTextStyle.Render("monthly")
		quarterView = inactiveTextStyle.Render("quarterly")
		yearView = textStyle.Render("yearly")
	}

	return lipgloss.JoinVertical(
		lipgloss.Right,
		sectionTitle,
		weekView,
		monthView,
		quarterView,
		yearView,
	)
}
