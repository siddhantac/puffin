package ui

import (
	"log"

	"github.com/siddhantac/puffin/ui/v2/interfaces"

	tea "github.com/charmbracelet/bubbletea"
	"github.com/charmbracelet/lipgloss"
)

type reports struct {
	incomeStatement     *complexTable
	balanceSheet        *complexTable
	dataProvider        interfaces.DataProvider
	filterGroup         *filterGroup
	displayOptionsGroup *displayOptionsGroup
	height, width       int
	cmdRunner           *cmdRunner
}

func newReports(dataProvider interfaces.DataProvider, cmdRunner *cmdRunner) *reports {
	optionFactory := displayOptionsGroupFactory{}
	filterGroupFactory := filterGroupFactory{}
	a := &reports{
		dataProvider:        dataProvider,
		filterGroup:         filterGroupFactory.NewGroupReports(),
		displayOptionsGroup: optionFactory.NewReportsGroup(interfaces.Yearly, 3, interfaces.ByAccount),
		cmdRunner:           cmdRunner,
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
	var cmd tea.Cmd
	switch msg := msg.(type) {
	case tea.WindowSizeMsg:
		log.Printf("reports: msg: %T", msg)
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

	case focusFilterMsg:
		log.Printf("reports: msg: %T", msg)
		a.filterGroup.Focus()
		return a, nil

	case blurFilterMsg:
		log.Printf("reports: msg: %T", msg)
		a.filterGroup.Blur()
		return a, nil

	case refreshDataMsg:
		log.Printf("reports: msg: %T", msg)
		a.filterGroup.Blur()
		return a, a.updateReportsCmd

	case tea.KeyMsg:
		log.Printf("reports: msg: %T | %v", msg, msg)
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
			dg, cmd := a.displayOptionsGroup.Update(msg)
			a.displayOptionsGroup = dg.(*displayOptionsGroup)
			if cmd != nil {
				return a, cmd
			}

			// standard table update so default keybindings work
			a.balanceSheet, _ = a.balanceSheet.Update(msg)
			a.incomeStatement, _ = a.incomeStatement.Update(msg)
			return a, nil
		}

	case updateReports:
		log.Printf("reports: msg: %T", msg)
		return a, tea.Batch(queryIncomeStatementCmd, queryBalanceSheetCmd)

	case queryIncomeStatement:
		a.incomeStatement.upper.SetReady(false)
		a.incomeStatement.lower.SetReady(false)
		log.Printf("reports: msg: %T", msg)
		f := func() tea.Msg {
			data := a.setIncomeStatementData()
			return updateIncomeStatement{data: data}
		}
		a.cmdRunner.Run(f)
		return a, nil

	case updateIncomeStatement:
		log.Printf("reports: msg: %T", msg)
		updateComplexTable(a.incomeStatement, msg.data, a.width)
		a.incomeStatement.upper.SetReady(true)
		a.incomeStatement.lower.SetReady(true)
		return a, nil

	case queryBalanceSheet:
		a.balanceSheet.upper.SetReady(false)
		a.balanceSheet.lower.SetReady(false)
		log.Printf("reports: msg: %T", msg)
		f := func() tea.Msg {
			data := a.setBalanceSheetData()
			return updateBalanceSheet{data: data}
		}
		a.cmdRunner.Run(f)
		return a, nil

	case updateBalanceSheet:
		log.Printf("reports: msg: %T", msg)
		updateComplexTable(a.balanceSheet, msg.data, a.width)
		a.balanceSheet.upper.SetReady(true)
		a.balanceSheet.lower.SetReady(true)
		return a, nil
	}

	return a, cmd
}

type queryIncomeStatement struct{}

func queryIncomeStatementCmd() tea.Msg {
	return queryIncomeStatement{}
}

type updateIncomeStatement struct {
	data *interfaces.ComplexTable
}

type queryBalanceSheet struct{}

func queryBalanceSheetCmd() tea.Msg {
	return queryBalanceSheet{}
}

type updateBalanceSheet struct {
	data *interfaces.ComplexTable
}

type updateReports struct{}

func (a *reports) updateReportsCmd() tea.Msg {
	return updateReports{}
}

func (a *reports) setIncomeStatementData() *interfaces.ComplexTable {
	filter := interfaces.Filter{
		Account:     a.filterGroup.AccountName(),
		DateStart:   a.filterGroup.DateStart(),
		DateEnd:     a.filterGroup.DateEnd(),
		Description: a.filterGroup.Description(),
	}

	displayOptions := interfaces.DisplayOptions{
		Interval: a.displayOptionsGroup.IntervalValue(),
		Depth:    a.displayOptionsGroup.DepthValue(),
		Sort:     a.displayOptionsGroup.SortValue(),
	}

	data, err := a.dataProvider.IncomeStatement(filter, displayOptions)
	if err != nil {
		log.Printf("error: %v", err)
		return nil
	}

	return data
}

func (a *reports) setBalanceSheetData() *interfaces.ComplexTable {
	filter := interfaces.Filter{
		Account:     a.filterGroup.AccountName(),
		DateStart:   a.filterGroup.DateStart(),
		DateEnd:     a.filterGroup.DateEnd(),
		Description: a.filterGroup.Description(),
	}

	displayOptions := interfaces.DisplayOptions{
		Interval: a.displayOptionsGroup.IntervalValue(),
		Depth:    a.displayOptionsGroup.DepthValue(),
		Sort:     a.displayOptionsGroup.SortValue(),
	}

	data, err := a.dataProvider.BalanceSheet(filter, displayOptions)
	if err != nil {
		log.Printf("error: %v", err)
		return nil
	}

	return data
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
			Render(divider.View()),
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
