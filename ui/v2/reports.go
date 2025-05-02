package ui

import (
	"log"
	"puffin/ui/v2/interfaces"

	"github.com/charmbracelet/bubbles/viewport"
	tea "github.com/charmbracelet/bubbletea"
	"github.com/charmbracelet/lipgloss"
)

type reports struct {
	incomeStatement     *complexTable
	balanceSheet        *complexTable
	dataProvider        interfaces.DataProvider
	filterGroup         *filterGroup
	displayOptionsGroup *displayOptionsGroup
	divider             viewport.Model
	height, width       int
}

func newReports(dataProvider interfaces.DataProvider) *reports {
	a := &reports{
		dataProvider:        dataProvider,
		filterGroup:         newFilterGroupAdvReports(),
		displayOptionsGroup: newDisplayOptionsGroup("yearly", 3, "acct"),
		divider:             viewport.New(1, 1),
	}
	a.newIncomeStatement()
	a.newBalanceSheet()
	a.incomeStatement.Focus()
	return a
}

func (a *reports) newIncomeStatement() {
	a.incomeStatement = newComplexTable()
	a.incomeStatement.upper.SetHeight((a.height - 20) / 2)
	a.incomeStatement.lower.SetHeight((a.height - 20) / 2)
	a.incomeStatement.bottomBar.SetHeight(1)
	a.incomeStatement.Init()
}

func (a *reports) newBalanceSheet() {
	a.balanceSheet = newComplexTable()
	a.balanceSheet.upper.SetHeight((a.height - 20) / 2)
	a.balanceSheet.lower.SetHeight((a.height - 20) / 2)
	a.balanceSheet.bottomBar.SetHeight(1)
	a.balanceSheet.Init()
}

func (a *reports) Init() tea.Cmd {
	return tea.Sequence(
		a.incomeStatement.Init(),
		a.balanceSheet.Init(),
		a.updateReportsCmd,
	)
}

func (a *reports) Update(msg tea.Msg) (tea.Model, tea.Cmd) {
	log.Printf("adv repo: msg: %T | %v", msg, msg)
	var cmd tea.Cmd
	switch msg := msg.(type) {
	case tea.WindowSizeMsg:
		a.height = msg.Height
		a.width = msg.Width

		fg, _ := a.filterGroup.Update(msg)
		a.filterGroup = fg.(*filterGroup)

		a.incomeStatement.upper.SetHeight((a.height - 20) / 2)
		a.incomeStatement.lower.SetHeight((a.height - 20) / 2)
		a.incomeStatement.bottomBar.SetHeight(1)

		a.balanceSheet.upper.SetHeight((a.height - 20) / 2)
		a.balanceSheet.lower.SetHeight((a.height - 20) / 2)
		a.balanceSheet.bottomBar.SetHeight(1)

		// only set column sizes, no update to row contents
		setColumns(a.incomeStatement, msg.Width)
		setColumns(a.balanceSheet, msg.Width)
		return a, nil

	case activateFilterMsg:
		a.filterGroup.Focus()
		return a, nil

	case cancelFilterMsg:
		a.filterGroup.Blur()
		return a, nil

	case applyFilterMsg:
		a.filterGroup.Blur()
		return a, a.updateReportsCmd

	case tea.KeyMsg:
		if a.filterGroup.Focused() {
			fg, cmd := a.filterGroup.Update(msg)
			a.filterGroup = fg.(*filterGroup)
			return a, cmd
		}
		switch msg.String() {
		case "1":
			a.incomeStatement.Focus()
			a.balanceSheet.Blur()
		case "2":
			a.incomeStatement.Blur()
			a.balanceSheet.Focus()

		default:
			dg, _ := a.displayOptionsGroup.Update(msg)
			a.displayOptionsGroup = dg.(*displayOptionsGroup)
			return a, a.updateReportsCmd
		}

	case updateReports:
		log.Printf("income statement update")
		a.setIncomeStatementData()
		a.setBalanceSheetData()
	}

	// standard table update so default keybindings work
	a.balanceSheet, cmd = a.balanceSheet.Update(msg)
	a.incomeStatement, cmd = a.incomeStatement.Update(msg)

	return a, cmd
}

type updateReports struct{}

func (a *reports) updateReportsCmd() tea.Msg {
	return updateReports{}
}

func (a *reports) setIncomeStatementData() {
	filter := interfaces.Filter{
		Account:     a.filterGroup.AccountName(),
		DateStart:   a.filterGroup.DateStart(),
		DateEnd:     a.filterGroup.DateEnd(),
		Description: a.filterGroup.Description(),
	}

	displayOptions := interfaces.DisplayOptions{
		Interval: a.displayOptionsGroup.IntervalValue(),
		Depth:    a.displayOptionsGroup.depth.value,
		Sort:     a.displayOptionsGroup.sort.value,
	}

	data, err := a.dataProvider.IncomeStatement(filter, displayOptions)
	if err != nil {
		log.Printf("error: %v", err)
		return
	}

	updateComplexTable(a.incomeStatement, data, a.width)
}

func (a *reports) setBalanceSheetData() {
	filter := interfaces.Filter{
		Account:     a.filterGroup.AccountName(),
		DateStart:   a.filterGroup.DateStart(),
		DateEnd:     a.filterGroup.DateEnd(),
		Description: a.filterGroup.Description(),
	}

	displayOptions := interfaces.DisplayOptions{
		Interval: a.displayOptionsGroup.IntervalValue(),
	}

	data, err := a.dataProvider.BalanceSheet(filter, displayOptions)
	if err != nil {
		log.Printf("error: %v", err)
		return
	}

	updateComplexTable(a.balanceSheet, data, a.width)
}

func (a *reports) View() string {
	var view string
	var title string

	if a.incomeStatement.Focused() {
		view = a.incomeStatement.View()
		title = lipgloss.JoinHorizontal(
			lipgloss.Top,
			activeTitleStyle.Render("(1) Income Statement"),
			inactiveTitleStyle.Render("(2) Balance Sheet"),
		)
	} else {
		view = a.balanceSheet.View()
		title = lipgloss.JoinHorizontal(
			lipgloss.Top,
			inactiveTitleStyle.Render("(1) Income Statement"),
			activeTitleStyle.Render("(2) Balance Sheet"),
		)
	}

	filterView := lipgloss.JoinHorizontal(
		lipgloss.Center,
		a.filterGroup.View(),
		" ",
		lipgloss.NewStyle().
			Border(lipgloss.RoundedBorder(), false, false, false, true).
			BorderForeground(lipgloss.Color("240")).
			Render(a.divider.View()),
		" ",
		a.displayOptionsGroup.View(),
	)
	return lipgloss.JoinVertical(
		lipgloss.Left,
		filterView,
		lipgloss.JoinVertical(
			lipgloss.Left,
			title,
			view,
		),
	)
}
