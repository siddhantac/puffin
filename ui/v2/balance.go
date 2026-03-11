package ui

import (
	"log"

	"github.com/charmbracelet/bubbles/table"
	tea "github.com/charmbracelet/bubbletea"
	"github.com/charmbracelet/lipgloss"
	"github.com/siddhantac/puffin/ui/v2/interfaces"
)

type queryAssetBalanceMsg struct{}

func queryAssetBalanceCmd() tea.Msg { return queryAssetBalanceMsg{} }

type queryBalanceMsg struct{}

func queryBalanceCmd() tea.Msg {
	return queryBalanceMsg{}
}

type balanceReports struct {
	height, width       int
	filterGroup         *filterGroup
	displayOptionsGroup *displayOptionsGroup
	dataProvider        interfaces.DataProvider
	cmdRunner           *cmdRunner

	assets      *customTable
	expenses    *customTable
	income      *customTable
	equity      *customTable
	liabilities *customTable

	tableTitles      []string
	tables           []*customTable
	activeTableIndex int
}

func newBalanceReports(dataProvider interfaces.DataProvider, cmdRunner *cmdRunner) *balanceReports {
	assetsTbl := newCustomTable("assets", "")
	assetsTbl.SetReady(true)
	assetsTbl.Focus()

	expensesTbl := newCustomTable("expenses", "")
	expensesTbl.SetReady(true)

	incomeTbl := newCustomTable("income", "")
	incomeTbl.SetReady(true)

	equityTbl := newCustomTable("equity", "")
	equityTbl.SetReady(true)

	liabilitiesTbl := newCustomTable("liabilities", "")
	liabilitiesTbl.SetReady(true)

	optionFactory := displayOptionsGroupFactory{}
	filterGroupFactory := filterGroupFactory{}
	br := &balanceReports{
		assets:              assetsTbl,
		expenses:            expensesTbl,
		income:              incomeTbl,
		equity:              equityTbl,
		liabilities:         liabilitiesTbl,
		dataProvider:        dataProvider,
		filterGroup:         filterGroupFactory.NewGroupBalance(),
		displayOptionsGroup: optionFactory.NewReportsGroup(interfaces.Yearly, 3, false),
		cmdRunner:           cmdRunner,
		tableTitles:         []string{"assets", "expenses", "income", "equity", "liabilities"},
		tables:              []*customTable{assetsTbl, expensesTbl, incomeTbl, equityTbl, liabilitiesTbl},
	}

	return br
}

func (b *balanceReports) Init() tea.Cmd {
	return tea.Sequence(
		b.assets.Init(),
		b.expenses.Init(),
		// queryBalanceCmd,
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
		b.assets.SetWidth(percent(b.width, 90))
		b.expenses.SetWidth(percent(b.width, 90))

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
		case "tab":
			for _, t := range b.tables {
				t.Blur()
			}

			n := len(b.tables)
			b.activeTableIndex = (b.activeTableIndex + 1) % n
			b.tables[b.activeTableIndex].Focus()
		case "shift+tab":
			for _, t := range b.tables {
				t.Blur()
			}

			n := len(b.tables)
			b.activeTableIndex = (b.activeTableIndex - 1 + n) % n
			b.tables[b.activeTableIndex].Focus()
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
		b.loadAll()
		return b, nil

	default:
		var cmds []tea.Cmd
		var cmd tea.Cmd
		for i, t := range b.tables {
			b.tables[i], cmd = t.Update(msg)
			cmds = append(cmds, cmd)
		}
		b.assets = b.tables[0]
		b.expenses = b.tables[1]
		b.income = b.tables[2]
		b.equity = b.tables[3]
		b.liabilities = b.tables[4]
		return b, tea.Batch(cmds...)
	}
}

func (b *balanceReports) loadAll() {
	for _, t := range b.tables {
		t.loadTable(b.cmdRunner, func() ([]table.Row, []table.Column) {
			return b.balanceData(t.name)
		})
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

	tableTitlesRendered := make([]string, 0)
	for idx := range b.tableTitles {
		var s string
		if idx == b.activeTableIndex {
			s = activeTabStyle.Render(b.tableTitles[idx])
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
		b.tables[b.activeTableIndex].View(),
	)
}

func calculateColumns(columnData []string, width int) []table.Column {
	accountColWidth := percent(width, 20)
	commodityColWidth := 10
	remainingWidth := width - accountColWidth - commodityColWidth - 2
	otherColumnsWidth := remainingWidth/(len(columnData)-2) - 2

	cols := []table.Column{
		{Title: "", Width: accountColWidth},
		{Title: columnData[1], Width: commodityColWidth},
	}
	for _, c := range columnData[2:] {
		cols = append(cols, table.Column{Title: c, Width: otherColumnsWidth})
	}
	return cols
}

func (b *balanceReports) balanceData(accountType string) ([]table.Row, []table.Column) {
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
		Average:  b.displayOptionsGroup.AverageValue(),
	}

	balanceData, err := b.dataProvider.Balance(filter, displayOptions)
	if err != nil {
		panic(err)
	}

	if len(balanceData) <= 1 {
		return nil, nil
	}

	cols := calculateColumns(balanceData[0], b.width)
	cols[0].Title = "account"

	data := balanceData[1:]
	rows := make([]table.Row, 0, len(data))
	for _, row := range data {
		rows = append(rows, row)
	}

	return rows, cols
}
