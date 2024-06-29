package ui

import (
	tea "github.com/charmbracelet/bubbletea"
	"github.com/siddhantac/hledger"
)

type Period struct {
	periodType hledger.PeriodType
}

func newPeriod(period hledger.PeriodType) *Period {
	return &Period{periodType: period}
}

func (p *Period) String() string {
	switch p.periodType {
	case hledger.PeriodWeekly:
		return "W"
	case hledger.PeriodMonthly:
		return "M"
	case hledger.PeriodQuarterly:
		return "Q"
	case hledger.PeriodYearly:
		return "Y"
	}
	return ""
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
