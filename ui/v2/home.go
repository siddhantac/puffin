package ui

import (
	"log"
	dataprovider "puffin/ui/v2/dataProvider"

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
}

func newHome() *home {
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
		register: regTbl,
		accounts: accTbl,
		balance:  balTbl,
	}
}

func (m *home) Init() tea.Cmd {
	return nil
}

func (m *home) Update(msg tea.Msg) (tea.Model, tea.Cmd) {
	log.Printf("home: msg: %T | %v", msg, msg)
	switch msg := msg.(type) {
	case tea.WindowSizeMsg:
		m.width = msg.Width
		m.height = msg.Height

		halfwidth := m.width/2 - 10
		m.accounts.SetWidth(halfwidth)
		m.register.SetWidth(halfwidth)
		m.balance.SetWidth(halfwidth)

		cols, rows := registerData(m.register.Width())
		m.register.SetColumns(cols)
		m.register.SetRows(rows)

		col2, row2 := accountsData(m.accounts.Width())
		m.accounts.SetColumns(col2)
		m.accounts.SetRows(row2)

		m.accounts.Focus()
		r := m.accounts.SelectedRow()
		m.selectedAccount = r[0]

		col3, row3 := balanceData(m.balance.Width(), m.selectedAccount)
		m.balance.SetColumns(col3)
		m.balance.SetRows(row3)

	case tea.KeyMsg:
		switch msg.String() {
		case "j":
			m.accounts.MoveDown(1)
			r := m.accounts.SelectedRow()
			m.selectedAccount = r[0]

			col3, row3 := balanceData(m.balance.Width(), m.selectedAccount)
			m.balance.SetColumns(col3)
			m.balance.SetRows(row3)
			return m, nil
		case "k":
			m.accounts.MoveUp(1)
			r := m.accounts.SelectedRow()
			m.selectedAccount = r[0]
			col3, row3 := balanceData(m.balance.Width(), m.selectedAccount)
			m.balance.SetColumns(col3)
			m.balance.SetRows(row3)
			return m, nil
		}
	}
	return m, nil
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

func registerData(width int) ([]table.Column, []table.Row) {
	return []table.Column{
			{Title: "date", Width: percent(width, 15)},
			{Title: "description", Width: percent(width, 35)},
			{Title: "account", Width: percent(width, 30)},
			{Title: "amount", Width: percent(width, 20)},
		}, []table.Row{
			{"2025-02-04", "First", "expense", "46"},
			{"2025-02-04", "Second", "assets", "100"},
			{"2025-02-04", "Third", "revenue", "51"},
			{"2025-02-04", "Fourth", "equity", "46"},
		}
}

func accountsData(width int) ([]table.Column, []table.Row) {
	balanceData, err := dataprovider.AccountBalances()
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
func balanceData(width int, account string) ([]table.Column, []table.Row) {
	balanceData, err := dataprovider.SubAccountBalances(account)
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
