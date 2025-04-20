package ui

import (
	"log"
	"puffin/ui/v2/interfaces"

	"github.com/charmbracelet/bubbles/table"
	tea "github.com/charmbracelet/bubbletea"
	"github.com/charmbracelet/lipgloss"
)

type advancedReports struct {
	incomeStatement     *complexTable
	balanceSheet        *complexTable
	dataProvider        interfaces.DataProvider
	filterGroup         *filterGroup
	displayOptionsGroup *displayOptionsGroup
	height, width       int
}

func newAdvancedReports(dataProvider interfaces.DataProvider) *advancedReports {
	a := &advancedReports{
		dataProvider:        dataProvider,
		filterGroup:         newFilterGroupAdvReports(),
		displayOptionsGroup: newDisplayOptionsGroup("yearly"),
	}
	a.newIncomeStatement()
	a.newBalanceSheet()
	a.incomeStatement.Focus()
	return a
}

func (a *advancedReports) newIncomeStatement() {
	a.incomeStatement = newComplexTable()
	a.incomeStatement.upper.SetHeight((a.height - 20) / 2)
	a.incomeStatement.lower.SetHeight((a.height - 20) / 2)
	a.incomeStatement.bottomBar.SetHeight(1)
	a.incomeStatement.Init()
}

func (a *advancedReports) newBalanceSheet() {
	a.balanceSheet = newComplexTable()
	a.balanceSheet.upper.SetHeight((a.height - 20) / 2)
	a.balanceSheet.lower.SetHeight((a.height - 20) / 2)
	a.balanceSheet.bottomBar.SetHeight(1)
	a.balanceSheet.Init()
}

func (a *advancedReports) Init() tea.Cmd {
	return tea.Sequence(
		a.incomeStatement.Init(),
		a.balanceSheet.Init(),
		a.updateReportsCmd,
	)
}

func (a *advancedReports) Update(msg tea.Msg) (tea.Model, tea.Cmd) {
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

		return a, a.updateReportsCmd

	case activateFilterMsg:
		a.filterGroup.Focus()
		return a, nil

	case tea.KeyMsg:
		if a.filterGroup.Focused() {
			if msg.String() == "esc" {
				a.filterGroup.Blur()
				return a, nil
			}
			if msg.String() == "enter" {
				a.filterGroup.Blur()
				return a, a.updateReportsCmd
			}

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

		case "m":
			a.displayOptionsGroup.interval.value = "monthly"
			return a, a.updateReportsCmd
		case "y":
			a.displayOptionsGroup.interval.value = "yearly"
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

func (a *advancedReports) updateReportsCmd() tea.Msg {
	return updateReports{}
}

func (a *advancedReports) setIncomeStatementData() {
	filter := interfaces.Filter{
		Account:     a.filterGroup.AccountName(),
		DateStart:   a.filterGroup.DateStart(),
		DateEnd:     a.filterGroup.DateEnd(),
		Description: a.filterGroup.Description(),
	}

	displayOptions := interfaces.DisplayOptions{
		Interval: a.displayOptionsGroup.interval.value,
	}

	data, err := a.dataProvider.IncomeStatement(filter, displayOptions)
	if err != nil {
		log.Printf("error: %v", err)
		return
	}

	updateComplexTable(a.incomeStatement, data, a.width)
}

func (a *advancedReports) setBalanceSheetData() {
	filter := interfaces.Filter{
		Account:     a.filterGroup.AccountName(),
		DateStart:   a.filterGroup.DateStart(),
		DateEnd:     a.filterGroup.DateEnd(),
		Description: a.filterGroup.Description(),
	}

	displayOptions := interfaces.DisplayOptions{
		Interval: a.displayOptionsGroup.interval.value,
	}

	data, err := a.dataProvider.BalanceSheet(filter, displayOptions)
	if err != nil {
		log.Printf("error: %v", err)
		return
	}

	updateComplexTable(a.balanceSheet, data, a.width)
}

func updateComplexTable(complexTable *complexTable, data *interfaces.ComplexTable, width int) {
	complexTable.title = data.Title
	complexTable.upperTitle = data.UpperTitle
	complexTable.lowerTitle = data.LowerTitle

	complexTable.upper.SetRows(nil)
	complexTable.lower.SetRows(nil)
	complexTable.bottomBar.SetRows(nil)

	accountColWidth := percent(width, 20)
	commodityColWidth := 10
	remainingWidth := width - accountColWidth - commodityColWidth - 2
	otherColumnsWidth := remainingWidth/(len(data.Columns)-2) - 2

	cols := []table.Column{
		{
			Title: data.Columns[1],
			Width: commodityColWidth,
		},
	}
	for _, c := range data.Columns[2:] {
		cols = append(cols,
			table.Column{
				Title: c,
				Width: otherColumnsWidth,
			})
	}

	revenueCols := make([]table.Column, 0)
	revenueCols = append(revenueCols, table.Column{
		Title: data.UpperTitle,
		Width: accountColWidth,
	},
	)
	revenueCols = append(revenueCols, cols...)
	complexTable.upper.SetColumns(revenueCols)

	expenseCols := []table.Column{
		{
			Title: data.LowerTitle,
			Width: accountColWidth,
		},
	}
	expenseCols = append(expenseCols, cols...)
	complexTable.lower.SetColumns(expenseCols)

	netCols := []table.Column{
		{
			Title: "Net",
			Width: accountColWidth,
		},
	}
	netCols = append(netCols, cols...)
	complexTable.bottomBar.SetColumns(netCols)

	upperRows := make([]table.Row, 0, len(data.Upper))
	for _, row := range data.Upper {
		upperRows = append(upperRows, row)
	}
	complexTable.upper.SetRows(upperRows)

	lowerRows := make([]table.Row, 0, len(data.Upper))
	for _, row := range data.Lower {
		lowerRows = append(lowerRows, row)
	}
	complexTable.lower.SetRows(lowerRows)

	complexTable.bottomBar.SetRows([]table.Row{data.BottomBar})
}

func (a *advancedReports) View() string {
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
		lipgloss.Top,
		a.filterGroup.View(),
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
