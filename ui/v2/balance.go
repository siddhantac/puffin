package ui

import (
	"log"

	"github.com/charmbracelet/bubbles/table"
	tea "github.com/charmbracelet/bubbletea"
	"github.com/charmbracelet/lipgloss"
	"github.com/siddhantac/puffin/ui/v2/interfaces"
)

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
	return &balanceReports{
		assets:              assetsTbl,
		dataProvider:        dataProvider,
		displayOptionsGroup: optionFactory.NewReportsGroup(interfaces.Yearly, 3, interfaces.ByAccount),
	}
}

func (b *balanceReports) Init() tea.Cmd {
	return nil
}

func (b *balanceReports) Update(msg tea.Msg) (tea.Model, tea.Cmd) {
	switch msg := msg.(type) {
	case tea.WindowSizeMsg:
		b.width = msg.Width
		b.height = msg.Height
		b.assets.SetHeight(msg.Height - 5)

		b.balanceData()

		return b, nil
	default:
		dg, cmd := b.displayOptionsGroup.Update(msg)
		b.displayOptionsGroup = dg.(*displayOptionsGroup)
		if cmd != nil {
			return b, cmd
		}

	}
	return b, nil
}

func (b *balanceReports) View() string {
	filterView := b.displayOptionsGroup.View()
	return lipgloss.JoinVertical(
		lipgloss.Left,
		filterView,
		b.assets.View(),
	)
}

func (b *balanceReports) balanceData() {

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
		return
	}

	cols := calculateColumns(balanceData[0], b.width)
	cols[0].Title = "account"
	b.assets.SetColumns(cols)
	log.Printf(">> %v", balanceData)

	data := balanceData[1:]
	rows := make([]table.Row, 0, len(data))
	for _, row := range data {
		rows = append(rows, row)
	}

	b.assets.SetRows(rows)
}
