package ui

//
// import (
// 	dataprovider "puffin/ui/v2/dataProvider"
//
// 	"github.com/charmbracelet/bubbles/table"
// 	tea "github.com/charmbracelet/bubbletea"
// 	"github.com/charmbracelet/lipgloss"
// )
//
// const (
// 	expensesAcct  = "expenses(x)"
// 	incomeAcct    = "revenue(r)"
// 	assetAcct     = "assets(a)"
// 	liabilityAcct = "liability(l)"
// 	equityAcct    = "equity(e)"
// )
//
// type account struct {
// 	register table.Model
// 	balance  table.Model
// }
//
// func (a account) View() string {
// 	return lipgloss.JoinHorizontal(
// 		lipgloss.Top,
// 		lipgloss.NewStyle().Border(lipgloss.RoundedBorder()).Render(a.balance.View()),
// 		lipgloss.NewStyle().Border(lipgloss.RoundedBorder()).Render(a.register.View()),
// 	)
// }
//
// type detailView struct {
// 	subNavBar    []string
// 	selectedItem string
//
// 	expenses  account
// 	income    account
// 	asset     account
// 	liability account
// 	equity    account
// }
//
// func newDetailView() *detailView {
// 	col, rows := registerData(100)
// 	regTbl := table.New(
// 		table.WithColumns(col),
// 		table.WithRows(rows),
// 		table.WithFocused(true),
// 		table.WithHeight(20),
// 	)
//
// 	col2, row2 := balanceData(50)
// 	balTbl := table.New(
// 		table.WithColumns(col2),
// 		table.WithRows(row2),
// 		table.WithFocused(true),
// 		table.WithHeight(6),
// 	)
//
// 	return &detailView{
// 		selectedItem: expensesAcct,
// 		subNavBar: []string{
// 			expensesAcct,
// 			incomeAcct,
// 			assetAcct,
// 			liabilityAcct,
// 			equityAcct,
// 		},
// 		expenses: account{
// 			register: regTbl,
// 			balance:  balTbl,
// 		},
// 	}
// }
//
// func (d *detailView) Init() tea.Cmd {
// 	return nil
// }
//
// func (d *detailView) Update(msg tea.Msg) (tea.Model, tea.Cmd) {
// 	switch msg := msg.(type) {
// 	case tea.KeyMsg:
// 		switch msg.String() {
// 		case "x":
// 			d.selectedItem = expensesAcct
// 		case "r":
// 			d.selectedItem = incomeAcct
// 		case "l":
// 			d.selectedItem = liabilityAcct
// 		case "a":
// 			d.selectedItem = assetAcct
// 		case "e":
// 			d.selectedItem = equityAcct
// 		}
// 	}
// 	return d, nil
// }
//
// func (d *detailView) View() string {
// 	renderedTabs := make([]string, 0)
// 	for _, t := range d.subNavBar {
// 		if t == d.selectedItem {
// 			renderedTabs = append(renderedTabs, activeTabStyle.Render(t))
// 		} else {
// 			renderedTabs = append(renderedTabs, inactiveTabStyle.Render(t))
// 		}
// 	}
// 	tabLine := lipgloss.JoinHorizontal(lipgloss.Top, renderedTabs...)
//
// 	navBarView := lipgloss.NewStyle().
// 		BorderStyle(lipgloss.NormalBorder()).
// 		BorderBottom(true).
// 		BorderForeground(lipgloss.Color("240")).
// 		Render(tabLine)
//
// 	return lipgloss.JoinVertical(
// 		lipgloss.Center,
// 		navBarView,
// 		d.expenses.View(),
// 	)
// }

// func balanceData(width int, account string) ([]table.Column, []table.Row) {
// 	balanceData, err := dataprovider.SubAccountBalances(account)
// 	if err != nil {
// 		panic(err)
// 	}
//
// 	header := balanceData[0]
// 	data := balanceData[1:]
// 	cols := []table.Column{
// 		{Title: header[0], Width: percent(width, 50)},
// 		{Title: header[1], Width: percent(width, 50)},
// 	}
//
// 	rows := make([]table.Row, 0, len(data))
// 	for _, row := range data {
// 		rows = append(rows, table.Row{row[0], row[1]})
// 	}
//
// 	return cols, rows
// }
