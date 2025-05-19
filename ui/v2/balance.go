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
	columns table.Row
}

func queryBalanceCmd() tea.Msg {
	return queryBalanceMsg{}
}

type balanceReports struct {
	height, width       int
	assets              table.Model
	filterGroup         *filterGroup
	displayOptionsGroup *displayOptionsGroup
	dataProvider        interfaces.DataProvider
}

func newBalanceReports(dataProvider interfaces.DataProvider) *balanceReports {
	assetsTbl := table.New(
		table.WithFocused(true),
	)

	optionFactory := displayOptionsGroupFactory{}
	filterGroupFactory := filterGroupFactory{}
	return &balanceReports{
		assets:              assetsTbl,
		dataProvider:        dataProvider,
		filterGroup:         filterGroupFactory.NewGroupBalance(),
		displayOptionsGroup: optionFactory.NewReportsGroup(interfaces.Yearly, 3, interfaces.ByAccount),
	}
}

func (b *balanceReports) Init() tea.Cmd {
	return queryBalanceCmd
}

func (b *balanceReports) Update(msg tea.Msg) (tea.Model, tea.Cmd) {
	switch msg := msg.(type) {
	case tea.WindowSizeMsg:
		b.width = msg.Width
		b.height = msg.Height

		fg, _ := b.filterGroup.Update(msg)
		b.filterGroup = fg.(*filterGroup)

		b.assets.SetHeight(msg.Height - 5)

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
		return b, b.updateBalanceCmd

	case updateBalanceMsg:
		b.assets.SetRows(msg.rows)
		return b, nil
	}
	return b, nil
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

func (b *balanceReports) updateBalanceCmd() tea.Msg {
	filter := interfaces.Filter{
		AccountType: "assets",
		// Account:     h.filterGroup.AccountName(),
		DateStart: "2025",
		// DateEnd:   h.filterGroup.DateEnd(),
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
	b.assets.SetColumns(cols)

	data := balanceData[1:]
	rows := make([]table.Row, 0, len(data))
	for _, row := range data {
		rows = append(rows, row)
	}
	//
	// b.assets.SetRows(rows)
	return updateBalanceMsg{rows: rows} //, columns: cols}
}
