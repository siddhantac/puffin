package ui

import (
	"puffin/ui/v2/interfaces"

	"github.com/charmbracelet/bubbles/viewport"
	tea "github.com/charmbracelet/bubbletea"
	"github.com/charmbracelet/lipgloss"
)

type advancedReports struct {
	incomeStatement viewport.Model
	dataProvider    interfaces.DataProvider
	filterGroup     *filterGroup
}

func newAdvancedReports(dataProvider interfaces.DataProvider) *advancedReports {
	return &advancedReports{
		incomeStatement: viewport.New(0, 0),
		dataProvider:    dataProvider,
		filterGroup:     newFilterGroupAdvReports(),
	}
}

func (a *advancedReports) Init() tea.Cmd {
	return nil
}

func (a *advancedReports) Update(msg tea.Msg) (tea.Model, tea.Cmd) {
	var cmd tea.Cmd
	switch msg := msg.(type) {
	case tea.WindowSizeMsg:
		a.incomeStatement = viewport.New(msg.Width, percent(msg.Height, 90))

		fg, cmd := a.filterGroup.Update(msg)
		a.filterGroup = fg.(*filterGroup)
		return a, cmd

	case tea.KeyMsg:
		if a.filterGroup.Focused() {
			if msg.String() == "esc" {
				a.filterGroup.Blur()
				// h.accounts.Focus()
				return a, nil
			}
			if msg.String() == "enter" {
				a.filterGroup.Blur()
				// a.accounts.Focus()
				return a, nil //h.updateBalanceTableCmd
			}

			fg, cmd := a.filterGroup.Update(msg)
			a.filterGroup = fg.(*filterGroup)
			return a, cmd
		}
		switch msg.String() {
		case "f":
			// a.incomeStatement.Blur()
			a.filterGroup.Focus()
			return a, nil
		}
	}
	a.incomeStatement, cmd = a.incomeStatement.Update(msg)

	filter := interfaces.Filter{
		Account:     a.filterGroup.AccountName(),
		DateStart:   a.filterGroup.DateStart(),
		DateEnd:     a.filterGroup.DateEnd(),
		Description: a.filterGroup.Description(),
	}
	data, err := a.dataProvider.IncomeStatement(filter)
	if err != nil {
		return a, nil
	}
	a.incomeStatement.SetContent(string(data))
	return a, cmd
}

func (a *advancedReports) View() string {
	return lipgloss.JoinVertical(
		lipgloss.Left,
		a.filterGroup.View(),
		a.incomeStatement.View(),
	)
}
