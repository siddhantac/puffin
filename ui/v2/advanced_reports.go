package ui

import (
	"log"
	"puffin/ui/v2/interfaces"

	"github.com/charmbracelet/bubbles/viewport"
	tea "github.com/charmbracelet/bubbletea"
	"github.com/charmbracelet/lipgloss"
)

var (
	activeTitleStyle   = lipgloss.NewStyle().Bold(true).Background(lipgloss.Color("57")).PaddingLeft(1).PaddingRight(1)
	inactiveTitleStyle = lipgloss.NewStyle().Bold(true).PaddingLeft(1).PaddingRight(1)
)

type updateIncomeStatement struct{}

type advancedReports struct {
	incomeStatement   viewport.Model
	balanceSheet      viewport.Model
	dataProvider      interfaces.DataProvider
	filterGroup       *filterGroup
	focusedModel      viewport.Model
	focusedModelTitle string
}

func newAdvancedReports(dataProvider interfaces.DataProvider) *advancedReports {
	return &advancedReports{
		incomeStatement: viewport.New(0, 0),
		balanceSheet:    viewport.New(0, 0),
		dataProvider:    dataProvider,
		filterGroup:     newFilterGroupAdvReports(),
	}
}

func (a *advancedReports) Init() tea.Cmd {
	return nil
}

func (a *advancedReports) Update(msg tea.Msg) (tea.Model, tea.Cmd) {
	log.Printf("adv repo: msg: %T | %v", msg, msg)
	var cmd tea.Cmd
	switch msg := msg.(type) {
	case tea.WindowSizeMsg:
		a.incomeStatement = viewport.New(msg.Width, percent(msg.Height, 88))
		a.setIncomeStatementData()
		a.focusedModel = a.incomeStatement
		a.focusedModelTitle = lipgloss.JoinHorizontal(
			lipgloss.Top,
			activeTitleStyle.Render("(1) Income Statement"),
			inactiveTitleStyle.Render("(2) Balance Sheet"),
		)

		a.balanceSheet = viewport.New(msg.Width, percent(msg.Height, 88))
		a.setBalanceSheetData()

		fg, cmd := a.filterGroup.Update(msg)
		a.filterGroup = fg.(*filterGroup)
		return a, tea.Sequence(
			a.updateIncomeStatementCmd,
			cmd,
		)

	case tea.KeyMsg:
		if a.filterGroup.Focused() {
			if msg.String() == "esc" {
				a.filterGroup.Blur()
				return a, nil
			}
			if msg.String() == "enter" {
				a.filterGroup.Blur()
				return a, a.updateIncomeStatementCmd
			}

			fg, cmd := a.filterGroup.Update(msg)
			a.filterGroup = fg.(*filterGroup)
			return a, cmd
		}
		switch msg.String() {
		case "f":
			a.filterGroup.Focus()
			return a, nil
		case "1":
			a.focusedModel = a.incomeStatement
			a.focusedModelTitle = lipgloss.JoinHorizontal(
				lipgloss.Top,
				activeTitleStyle.Render("(1) Income Statement"),
				inactiveTitleStyle.Render("(2) Balance Sheet"),
			)
		case "2":
			a.focusedModel = a.balanceSheet
			a.focusedModelTitle = lipgloss.JoinHorizontal(
				lipgloss.Top,
				inactiveTitleStyle.Render("(1) Income Statement"),
				activeTitleStyle.Render("(2) Balance Sheet"),
			)
		}

	case updateIncomeStatement:
		log.Printf("income statement update")
		a.setIncomeStatementData()
		a.setBalanceSheetData()
	}
	a.incomeStatement, cmd = a.incomeStatement.Update(msg)
	a.balanceSheet, cmd = a.balanceSheet.Update(msg)

	return a, cmd
}

func (a *advancedReports) updateIncomeStatementCmd() tea.Msg {
	return updateIncomeStatement{}
}

func (a *advancedReports) setIncomeStatementData() {
	filter := interfaces.Filter{
		Account:     a.filterGroup.AccountName(),
		DateStart:   a.filterGroup.DateStart(),
		DateEnd:     a.filterGroup.DateEnd(),
		Description: a.filterGroup.Description(),
	}
	data, err := a.dataProvider.IncomeStatement(filter)
	if err != nil {
		return
	}
	a.incomeStatement.SetContent(string(data))
}

func (a *advancedReports) setBalanceSheetData() {
	filter := interfaces.Filter{
		Account:     a.filterGroup.AccountName(),
		DateStart:   a.filterGroup.DateStart(),
		DateEnd:     a.filterGroup.DateEnd(),
		Description: a.filterGroup.Description(),
	}
	data, err := a.dataProvider.BalanceSheet(filter)
	if err != nil {
		return
	}
	a.balanceSheet.SetContent(string(data))
}

func (a *advancedReports) View() string {
	incStmtStyle := lipgloss.NewStyle().PaddingLeft(2)
	return lipgloss.JoinVertical(
		lipgloss.Left,
		a.filterGroup.View(),
		lipgloss.JoinVertical(
			lipgloss.Left,
			a.focusedModelTitle,
			incStmtStyle.Render(a.focusedModel.View()),
		),
	)
}
