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
	rows    []table.Row
	columns []table.Column
}

func queryBalanceCmd() tea.Msg {
	return queryBalanceMsg{}
}

type balanceReports struct {
	height, width       int
	assets              *customTable
	filterGroup         *filterGroup
	displayOptionsGroup *displayOptionsGroup
	dataProvider        interfaces.DataProvider
	cmdRunner           *cmdRunner
}

func newBalanceReports(dataProvider interfaces.DataProvider, cmdRunner *cmdRunner) *balanceReports {
	assetsTbl := newCustomTable("assets")
	assetsTbl.SetReady(true)
	assetsTbl.Focus()

	optionFactory := displayOptionsGroupFactory{}
	filterGroupFactory := filterGroupFactory{}
	br := &balanceReports{
		assets:              assetsTbl,
		dataProvider:        dataProvider,
		filterGroup:         filterGroupFactory.NewGroupBalance(),
		displayOptionsGroup: optionFactory.NewReportsGroup(interfaces.Yearly, 3, interfaces.ByAccount),
		cmdRunner:           cmdRunner,
	}

	return br
}

func (b *balanceReports) Init() tea.Cmd {
	return tea.Sequence(
		b.assets.Init(),
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
		return b, nil

	case queryBalanceMsg:
		b.assets.SetReady(false)
		f := func() tea.Msg {
			return b.balanceData()
		}
		b.cmdRunner.Run(f)
		return b, nil

	case updateBalanceMsg:
		b.assets.SetRows(nil)
		b.assets.SetColumns(msg.columns)
		b.assets.SetRows(msg.rows)
		b.assets.SetReady(true)
		b.assets.SetCursor(0)
		return b, nil

	default:
		var cmd tea.Cmd
		b.assets, cmd = b.assets.Update(msg)
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
	return lipgloss.JoinVertical(
		lipgloss.Left,
		filterView,
		b.assets.View(),
	)
}

func (b *balanceReports) balanceData() updateBalanceMsg {
	filter := interfaces.Filter{
		AccountType: "assets",
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
		rows:    rows,
		columns: cols,
	}
}
