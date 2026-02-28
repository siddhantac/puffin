package ui

import (
	"log"

	"github.com/charmbracelet/bubbles/table"
	tea "github.com/charmbracelet/bubbletea"
	"github.com/charmbracelet/lipgloss"
	"github.com/siddhantac/puffin/ui/v2/interfaces"
)

type queryBalanceMsg struct{}
type updateBalanceMsg struct {
	accountType string
	rows        []table.Row
	columns     []table.Column
}

func queryBalanceCmd() tea.Msg {
	return queryBalanceMsg{}
}

type balanceReports struct {
	height, width       int
	filterGroup         *filterGroup
	displayOptionsGroup *displayOptionsGroup
	dataProvider        interfaces.DataProvider
	cmdRunner           *cmdRunner

	assets                *customTable
	expenses              *customTable
	activeTable           *customTable
	tableTitles           []string
	activeTableTitleIndex int
}

func newBalanceReports(dataProvider interfaces.DataProvider, cmdRunner *cmdRunner) *balanceReports {
	assetsTbl := newCustomTable("")
	assetsTbl.SetReady(true)
	assetsTbl.Focus()

	expensesTbl := newCustomTable("")
	expensesTbl.SetReady(true)

	optionFactory := displayOptionsGroupFactory{}
	filterGroupFactory := filterGroupFactory{}
	br := &balanceReports{
		assets:              assetsTbl,
		expenses:            expensesTbl,
		dataProvider:        dataProvider,
		filterGroup:         filterGroupFactory.NewGroupBalance(),
		displayOptionsGroup: optionFactory.NewReportsGroup(interfaces.Yearly, 3, interfaces.ByAccount),
		cmdRunner:           cmdRunner,
		tableTitles:         []string{"(1) assets", "(2) expenses"},
		activeTable:         assetsTbl,
	}

	return br
}

func (b *balanceReports) Init() tea.Cmd {
	return tea.Sequence(
		b.assets.Init(),
		b.expenses.Init(),
		queryBalanceCmd,
	)
}

func (b *balanceReports) Update(msg tea.Msg) (tea.Model, tea.Cmd) {
	switch msg := msg.(type) {
	case tea.WindowSizeMsg:
		b.width = msg.Width
		b.height = msg.Height

		fg, _ := b.filterGroup.Update(msg)
		b.filterGroup = fg.(*filterGroup)

		b.assets.SetHeight(msg.Height - 11)
		b.expenses.SetHeight(msg.Height - 11)

		return b, nil

	case focusFilterMsg:
		log.Printf("balances: msg: %T", msg)
		b.filterGroup.Focus()
		return b, nil

	case blurFilterMsg:
		log.Printf("balances: msg: %T", msg)
		b.filterGroup.Blur()
		return b, nil

	case refreshDataMsg:
		log.Printf("balances: msg: %T", msg)
		b.filterGroup.Blur()
		return b, queryBalanceCmd

	case tea.KeyMsg:
		log.Printf("balances: msg: %T | %v", msg, msg)
		switch msg.String() {
		case "1":
			b.assets.Focus()
			b.expenses.Blur()
			b.activeTable = b.assets
			b.activeTableTitleIndex = 0
		case "2":
			b.assets.Blur()
			b.expenses.Focus()
			b.activeTable = b.expenses
			b.activeTableTitleIndex = 1
		}

		if msg.Type == tea.KeyEnter {
			if b.filterGroup.Focused() {
				return b, queryBalanceCmd
			}
		}

		if b.filterGroup.Focused() {
			fg, cmd := b.filterGroup.Update(msg)
			b.filterGroup = fg.(*filterGroup)
			return b, cmd
		}

		dg, cmd := b.displayOptionsGroup.Update(msg)
		b.displayOptionsGroup = dg.(*displayOptionsGroup)
		if cmd != nil {
			return b, cmd
		}

		b.assets, _ = b.assets.Update(msg)
		b.expenses, _ = b.expenses.Update(msg)
		return b, nil

	case queryBalanceMsg:
		b.assets.SetReady(false)
		b.expenses.SetReady(false)
		f := func() tea.Msg {
			return b.balanceData("assets")
		}
		b.cmdRunner.Run(f)
		f = func() tea.Msg {
			return b.balanceData("expenses")
		}
		b.cmdRunner.Run(f)
		return b, nil

	case updateBalanceMsg:
		switch msg.accountType {
		case "assets":
			b.assets.SetRows(nil)
			b.assets.SetColumns(msg.columns)
			b.assets.SetRows(msg.rows)
			b.assets.SetReady(true)
			b.assets.SetCursor(0)

		case "expenses":
			b.expenses.SetRows(nil)
			b.expenses.SetColumns(msg.columns)
			b.expenses.SetRows(msg.rows)
			b.expenses.SetReady(true)
			b.expenses.SetCursor(0)
		}
		return b, nil

	default:
		var cmd tea.Cmd
		b.assets, cmd = b.assets.Update(msg)
		b.expenses, cmd = b.expenses.Update(msg)
		return b, cmd
	}
}

func (b *balanceReports) View() string {
	filterView := lipgloss.JoinHorizontal(
		lipgloss.Center,
		b.filterGroup.View(),
		" ",
		lipgloss.NewStyle().
			Border(lipgloss.RoundedBorder(), false, false, false, true).
			BorderForeground(lipgloss.Color("240")).
			Render(divider.View()),
		" ",
		b.displayOptionsGroup.View(),
	)

	tableTitleStyle := lipgloss.NewStyle().PaddingLeft(1).PaddingRight(1)
	activeTableTitleStyle := tableTitleStyle.Copy().Background(lipgloss.Color("57"))

	tableTitlesRendered := make([]string, 0)
	for idx := range b.tableTitles {
		var s string
		if idx == b.activeTableTitleIndex {
			s = activeTableTitleStyle.Render(b.tableTitles[idx])
		} else {
			s = tableTitleStyle.Render(b.tableTitles[idx])
		}

		tableTitlesRendered = append(tableTitlesRendered, s)
	}

	return lipgloss.JoinVertical(
		lipgloss.Left,
		filterView,
		lipgloss.JoinHorizontal(
			lipgloss.Top,
			tableTitlesRendered...,
		),
		b.activeTable.View(),
		// lipgloss.JoinHorizontal(
		// 	lipgloss.Top,
		// 	b.assets.View(),
		// 	b.expenses.View(),
		// ),
	)
}

func (b *balanceReports) assetBalanceData() updateBalanceMsg {
	return b.balanceData("assets")
}

func (b *balanceReports) balanceData(accountType string) updateBalanceMsg {
	filter := interfaces.Filter{
		AccountType: accountType,
		Account:     b.filterGroup.AccountName(),
		DateStart:   b.filterGroup.DateStart(),
		DateEnd:     b.filterGroup.DateEnd(),
	}

	displayOptions := interfaces.DisplayOptions{
		Interval: b.displayOptionsGroup.IntervalValue(),
		Depth:    b.displayOptionsGroup.DepthValue(),
		Sort:     b.displayOptionsGroup.SortValue(),
	}

	balanceData, err := b.dataProvider.Balance(filter, displayOptions)
	if err != nil {
		panic(err)
	}

	if len(balanceData) <= 1 {
		return updateBalanceMsg{}
	}

	cols := calculateColumns(balanceData[0], b.width)

	cols[0].Title = "account"

	data := balanceData[1:]
	rows := make([]table.Row, 0, len(data))
	for _, row := range data {
		rows = append(rows, row)
	}

	return updateBalanceMsg{
		accountType: accountType,
		rows:        rows,
		columns:     cols,
	}
}
