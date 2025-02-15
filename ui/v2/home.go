package ui

import (
	"log"

	"puffin/ui/v2/interfaces"

	"github.com/charmbracelet/bubbles/table"
	tea "github.com/charmbracelet/bubbletea"
	"github.com/charmbracelet/lipgloss"
)

type home struct {
	height, width int
	register      table.Model
	accounts      table.Model
	balance       table.Model

	selectedAccount string
	dataProvider    interfaces.DataProvider
}

func newHome(dataProvider interfaces.DataProvider) *home {
	regTbl := table.New(
		table.WithFocused(true),
		table.WithHeight(20),
	)

	accTbl := table.New(
		table.WithFocused(true),
		table.WithHeight(6),
	)

	balTbl := table.New(
		table.WithFocused(true),
		table.WithHeight(6),
	)

	return &home{
		register:     regTbl,
		accounts:     accTbl,
		balance:      balTbl,
		dataProvider: dataProvider,
	}
}

func (m *home) Init() tea.Cmd {
	return nil
}

func (h *home) Update(msg tea.Msg) (tea.Model, tea.Cmd) {
	log.Printf("home: msg: %T | %v", msg, msg)
	switch msg := msg.(type) {
	case tea.WindowSizeMsg:
		h.width = msg.Width
		h.height = msg.Height

		halfwidth := h.width/2 - 10
		h.accounts.SetWidth(halfwidth)
		h.register.SetWidth(halfwidth)
		h.balance.SetWidth(halfwidth)

		col2, row2 := h.accountsData(h.accounts.Width())
		h.accounts.SetColumns(col2)
		h.accounts.SetRows(row2)

		h.accounts.Focus()
		r := h.accounts.SelectedRow()
		h.selectedAccount = r[0]

		col3, row3 := h.balanceData(h.balance.Width(), h.selectedAccount)
		h.balance.SetColumns(col3)
		h.balance.SetRows(row3)

		cols, rows := h.registerData(h.register.Width(), h.selectedAccount)
		h.register.SetColumns(cols)
		h.register.SetRows(rows)

	case tea.KeyMsg:
		switch msg.String() {
		case "j":
			h.accounts.MoveDown(1)
			r := h.accounts.SelectedRow()
			h.selectedAccount = r[0]

			col3, row3 := h.balanceData(h.balance.Width(), h.selectedAccount)
			h.balance.SetColumns(col3)
			h.balance.SetRows(row3)
			cols, rows := h.registerData(h.register.Width(), h.selectedAccount)
			h.register.SetColumns(cols)
			h.register.SetRows(rows)
			return h, nil
		case "k":
			h.accounts.MoveUp(1)
			r := h.accounts.SelectedRow()
			h.selectedAccount = r[0]
			col3, row3 := h.balanceData(h.balance.Width(), h.selectedAccount)
			h.balance.SetColumns(col3)
			h.balance.SetRows(row3)
			cols, rows := h.registerData(h.register.Width(), h.selectedAccount)
			h.register.SetColumns(cols)
			h.register.SetRows(rows)
			return h, nil
		}
	}
	return h, nil
}

func (m *home) View() string {
	left := lipgloss.JoinVertical(
		lipgloss.Left,
		lipgloss.NewStyle().Border(lipgloss.RoundedBorder()).Render(m.accounts.View()),
		lipgloss.NewStyle().Border(lipgloss.RoundedBorder()).Render(m.balance.View()),
	)
	right := lipgloss.NewStyle().Border(lipgloss.RoundedBorder()).Render(m.register.View())

	content := lipgloss.JoinHorizontal(
		lipgloss.Top,
		left,
		right,
	)
	return content
}

func percent(number, percentage int) int {
	return (percentage * number) / 100
}

func (h *home) registerData(width int, account string) ([]table.Column, []table.Row) {
	registerData, err := h.dataProvider.Records(account)
	if err != nil {
		panic(err)
	}

	header := registerData[0]
	data := registerData[1:]
	cols := []table.Column{
		{Title: header[0], Width: percent(width, 15)},
		{Title: header[1], Width: percent(width, 35)},
		{Title: header[2], Width: percent(width, 30)},
		{Title: header[3], Width: percent(width, 20)},
	}

	rows := make([]table.Row, 0, len(data))
	for _, row := range data {
		rows = append(rows, row)
	}

	return cols, rows
}

func (h *home) accountsData(width int) ([]table.Column, []table.Row) {
	balanceData, err := h.dataProvider.AccountBalances()
	if err != nil {
		panic(err)
	}

	header := balanceData[0]
	data := balanceData[1:]
	cols := []table.Column{
		{Title: header[0], Width: percent(width, 50)},
		{Title: header[1], Width: percent(width, 50)},
	}

	rows := make([]table.Row, 0, len(data))
	for _, row := range data {
		rows = append(rows, table.Row{row[0], row[1]})
	}

	return cols, rows
}
func (h *home) balanceData(width int, account string) ([]table.Column, []table.Row) {
	balanceData, err := h.dataProvider.SubAccountBalances(account)
	if err != nil {
		panic(err)
	}

	header := balanceData[0]
	data := balanceData[1:]
	cols := []table.Column{
		{Title: header[0], Width: percent(width, 50)},
		{Title: header[1], Width: percent(width, 50)},
	}

	rows := make([]table.Row, 0, len(data))
	for _, row := range data {
		rows = append(rows, table.Row{row[0], row[1]})
	}

	return cols, rows
}

func commoditiesData(width int) ([]table.Column, []table.Row) {
	return []table.Column{
			{Title: "Commodities", Width: width},
		}, []table.Row{
			{"SGD$"},
			{"EUR"},
			{"USD$"},
			{"INR"},
		}
}

const stats = `Main file           : .../finances.journal
Included files      : 129
Txns span           : 2020-08-01 to 2025-12-31 (1978 days)
Last txn            : 2025-12-30 (328 days from now)
Txns                : 8641 (4.4 per day)
Txns last 30 days   : 25 (0.8 per day)
Txns last 7 days    : 0 (0.0 per day)
Payees/descriptions : 3480
Accounts            : 101 (depth 4)
Commodities         : 2
Market prices       : 0
Runtime stats       : 0.42 s elapsed, 20533 txns/s, 25 MB live, 77 MB alloc
`
