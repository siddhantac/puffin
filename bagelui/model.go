package bagelui

import (
	"fmt"
	"os"

	"github.com/charmbracelet/bubbles/table"
	"github.com/charmbracelet/bubbles/viewport"
	tea "github.com/charmbracelet/bubbletea"
	"github.com/charmbracelet/lipgloss"
)

func Start() {
	p := tea.NewProgram(newModel())
	if _, err := p.Run(); err != nil {
		fmt.Println("Error running program:", err)
		os.Exit(1)
	}
}

type model struct {
	statistics viewport.Model
	register   table.Model
	accounts   table.Model
}

func newModel() *model {
	col, rows := registerData()
	regTbl := table.New(
		table.WithColumns(col),
		table.WithRows(rows),
		table.WithFocused(true),
		table.WithHeight(20),
	)

	col2, row2 := accountsData()
	accTbl := table.New(
		table.WithColumns(col2),
		table.WithRows(row2),
		table.WithFocused(true),
		table.WithHeight(6),
	)

	statistics := viewport.New(45, 12)
	statistics.SetContent(stats)

	return &model{
		statistics: statistics,
		register:   regTbl,
		accounts:   accTbl,
	}
}

func (m *model) Init() tea.Cmd {
	return tea.EnterAltScreen
}

func (m *model) Update(msg tea.Msg) (tea.Model, tea.Cmd) {
	switch msg := msg.(type) {
	case tea.KeyMsg:
		switch msg.String() {
		// case key.Matches(msg, keys.Quit):
		case "q":
			return m, tea.Quit
		}
	}

	return m, nil
}

func (m *model) View() string {
	left := lipgloss.JoinVertical(
		lipgloss.Left,
		lipgloss.NewStyle().Border(lipgloss.RoundedBorder()).Render(m.accounts.View()),
		lipgloss.NewStyle().Border(lipgloss.RoundedBorder()).Render(m.statistics.View()),
	)
	right := lipgloss.NewStyle().Border(lipgloss.RoundedBorder()).Render(m.register.View())
	content := lipgloss.JoinHorizontal(
		lipgloss.Top,
		left,
		right,
	)
	return content
}

func registerData() ([]table.Column, []table.Row) {
	return []table.Column{
			{Title: "date", Width: 10},
			{Title: "description", Width: 10},
			{Title: "account", Width: 10},
			{Title: "amount", Width: 10},
		}, []table.Row{
			{"2025-02-04", "First", "expense", "46"},
			{"2025-02-04", "Second", "assets", "100"},
			{"2025-02-04", "Third", "revenue", "51"},
			{"2025-02-04", "Fourth", "equity", "46"},
		}
}

func accountsData() ([]table.Column, []table.Row) {
	return []table.Column{
			{Title: "Account", Width: 15},
			{Title: "Balance", Width: 15},
		}, []table.Row{
			{"expenses", "-1230"},
			{"revenue", "310"},
			{"equity", "100"},
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
