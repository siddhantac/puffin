package ui

import (
	"log"
	"puffin/ui/v2/interfaces"

	"github.com/charmbracelet/bubbles/table"
	"github.com/charmbracelet/bubbles/viewport"
	tea "github.com/charmbracelet/bubbletea"
	"github.com/charmbracelet/lipgloss"
)

type advancedReports struct {
	incomeStatement   *complexTable
	balanceSheet      viewport.Model
	dataProvider      interfaces.DataProvider
	filterGroup       *filterGroup
	focusedModel      viewport.Model
	focusedModelTitle string
	height, width     int
}

func newAdvancedReports(dataProvider interfaces.DataProvider) *advancedReports {
	a := &advancedReports{
		dataProvider: dataProvider,
		filterGroup:  newFilterGroupAdvReports(),
		balanceSheet: viewport.New(0, 0),
	}
	a.newIncomeStatement()
	return a
}

func (a *advancedReports) newIncomeStatement() {
	a.incomeStatement = newComplexTable()
	a.incomeStatement.upper.SetHeight((a.height - 20) / 2)
	a.incomeStatement.lower.SetHeight((a.height - 20) / 2)
	a.incomeStatement.bottomBar.SetHeight(1)
}

func (a *advancedReports) Init() tea.Cmd {
	return a.incomeStatement.Init()
}

func (a *advancedReports) Update(msg tea.Msg) (tea.Model, tea.Cmd) {
	log.Printf("adv repo: msg: %T | %v", msg, msg)
	var cmd tea.Cmd
	switch msg := msg.(type) {
	case tea.WindowSizeMsg:
		a.height = msg.Height
		a.width = msg.Width

		a.setIncomeStatementData()
		a.focusedModelTitle = lipgloss.JoinHorizontal(
			lipgloss.Top,
			activeTitleStyle.Render("(1) Income Statement"),
			inactiveTitleStyle.Render("(2) Balance Sheet"),
		)

		a.balanceSheet = viewport.New(msg.Width, percent(msg.Height, 88))
		a.setBalanceSheetData()

		fg, cmd := a.filterGroup.Update(msg)
		a.filterGroup = fg.(*filterGroup)

		a.incomeStatement.upper.SetHeight((a.height - 20) / 2)
		a.incomeStatement.lower.SetHeight((a.height - 20) / 2)
		a.incomeStatement.bottomBar.SetHeight(1)

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
	a.balanceSheet, cmd = a.balanceSheet.Update(msg)
	a.incomeStatement, cmd = a.incomeStatement.Update(msg)

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

	complexTable, err := a.dataProvider.IncomeStatement2(filter)
	if err != nil {
		log.Printf("error: %v", err)
		return
	}

	a.newIncomeStatement()
	a.incomeStatement.title = complexTable.Title
	a.incomeStatement.upperTitle = complexTable.UpperTitle
	a.incomeStatement.lowerTitle = complexTable.LowerTitle

	accountColWidth := percent(a.width, 20)
	commodityColWidth := 10
	remainingWidth := a.width - accountColWidth - commodityColWidth - 2
	otherColumnsWidth := remainingWidth/(len(complexTable.Columns)-2) - 2

	cols := []table.Column{
		{
			Title: complexTable.Columns[1],
			Width: commodityColWidth,
		},
	}
	for _, c := range complexTable.Columns[2:] {
		cols = append(cols,
			table.Column{
				Title: c,
				Width: otherColumnsWidth,
			})
	}

	revenueCols := make([]table.Column, 0)
	revenueCols = append(revenueCols, table.Column{
		Title: "revenue/income",
		Width: accountColWidth,
	},
	)
	revenueCols = append(revenueCols, cols...)
	log.Printf("%v", revenueCols)
	a.incomeStatement.upper.SetColumns(revenueCols)

	expenseCols := []table.Column{
		{
			Title: "expenses",
			Width: accountColWidth,
		},
	}
	expenseCols = append(expenseCols, cols...)
	a.incomeStatement.lower.SetColumns(expenseCols)

	netCols := []table.Column{
		{
			Title: "Net",
			Width: accountColWidth,
		},
	}
	netCols = append(netCols, cols...)
	a.incomeStatement.bottomBar.SetColumns(netCols)

	upperRows := make([]table.Row, 0, len(complexTable.Upper))
	for _, row := range complexTable.Upper {
		upperRows = append(upperRows, row)
	}
	a.incomeStatement.upper.SetRows(upperRows)

	lowerRows := make([]table.Row, 0, len(complexTable.Upper))
	for _, row := range complexTable.Lower {
		lowerRows = append(lowerRows, row)
	}
	a.incomeStatement.lower.SetRows(lowerRows)

	a.incomeStatement.bottomBar.SetRows([]table.Row{complexTable.BottomBar})
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

	// s := lipgloss.NewStyle().PaddingLeft(2)
	return lipgloss.JoinVertical(
		lipgloss.Left,
		a.filterGroup.View(),
		lipgloss.JoinVertical(
			lipgloss.Left,
			a.focusedModelTitle,
			a.incomeStatement.View(),
		),
	)
}
