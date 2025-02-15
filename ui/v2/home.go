package ui

import (
	"log"

	"github.com/charmbracelet/bubbles/table"
	"github.com/charmbracelet/bubbles/viewport"
	tea "github.com/charmbracelet/bubbletea"
	"github.com/charmbracelet/lipgloss"
)

type home struct {
	height, width int
	register      table.Model
	accounts      table.Model
	statistics    viewport.Model
	commodities   table.Model
}

func newHome() *home {
	// col, rows := registerData()
	regTbl := table.New(
		// table.WithColumns(col),
		// table.WithRows(rows),
		table.WithFocused(true),
		table.WithHeight(20),
	)

	// col2, row2 := accountsData()
	accTbl := table.New(
		// table.WithColumns(col2),
		// table.WithRows(row2),
		table.WithFocused(true),
		table.WithHeight(6),
	)

	return &home{
		register:    regTbl,
		accounts:    accTbl,
		statistics:  viewport.Model{},
		commodities: table.New(),
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

		halfwidth := m.width / 2
		m.accounts.SetWidth(halfwidth)
		m.register.SetWidth(m.width - 10)
		m.commodities.SetWidth(halfwidth)

		cols, rows := registerData(m.register.Width())
		m.register.SetColumns(cols)
		m.register.SetRows(rows)

		col2, row2 := accountsData(m.accounts.Width())
		m.accounts.SetColumns(col2)
		m.accounts.SetRows(row2)

		m.statistics = viewport.New(halfwidth+10, 12)
		m.statistics.SetContent(stats)

		col3, row3 := commoditiesData(m.commodities.Width())
		m.commodities.SetColumns(col3)
		m.commodities.SetRows(row3)
		m.accounts.SetHeight(11 - len(row3))
	}
	return m, nil
}

func (m *home) View() string {
	top := lipgloss.JoinHorizontal(
		lipgloss.Top,
		lipgloss.JoinVertical(
			lipgloss.Left,
			lipgloss.NewStyle().Border(lipgloss.RoundedBorder()).Render(m.accounts.View()),
			// lipgloss.NewStyle().Border(lipgloss.RoundedBorder()).Render(m.commodities.View()),
		),
		lipgloss.NewStyle().Border(lipgloss.RoundedBorder()).
			PaddingRight(1).
			PaddingLeft(1).
			Render(m.statistics.View()),
	)
	bottom := lipgloss.NewStyle().Border(lipgloss.RoundedBorder()).Render(m.register.View())
	content := lipgloss.JoinVertical(
		lipgloss.Left,
		top,
		bottom,
	)
	return content
}

// func (m *home) View() string {
// 	left := lipgloss.JoinVertical(
// 		lipgloss.Left,
// 		lipgloss.NewStyle().Border(lipgloss.RoundedBorder()).Render(m.accounts.View()),
// 		lipgloss.NewStyle().Border(lipgloss.RoundedBorder()).Render(m.statistics.View()),
// 	)
// 	right := lipgloss.NewStyle().Border(lipgloss.RoundedBorder()).Render(m.register.View())
// 	content := lipgloss.JoinHorizontal(
// 		lipgloss.Top,
// 		left,
// 		right,
// 	)
// 	return content
// }

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
	return []table.Column{
			{Title: "Account", Width: percent(width, 40)},
			{Title: "Balance", Width: percent(width, 40)},
		}, []table.Row{
			{"expenses", "-1230"},
			{"revenue", "310"},
			{"equity", "100"},
			{"liabilities", "100"},
			{"assets", "100"},
		}
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
