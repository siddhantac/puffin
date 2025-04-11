package ui

import (
	"fmt"
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
	filterGroup   *filterGroup

	selectedAccount    string
	selectedSubAccount string
	dataProvider       interfaces.DataProvider
}

func newHome(dataProvider interfaces.DataProvider) *home {
	regTbl := table.New(
		table.WithHeight(20),
	)

	accTbl := table.New(
		table.WithFocused(true),
		table.WithHeight(6),
	)

	balTbl := table.New(
		table.WithHeight(6),
	)

	return &home{
		register:     regTbl,
		accounts:     accTbl,
		balance:      balTbl,
		dataProvider: dataProvider,
		filterGroup:  newFilterGroupHome(),
	}
}

type updateBalance struct {
	account string
}

type updateRegister struct {
	subAccount string
}

func (m *home) Init() tea.Cmd {
	return m.filterGroup.Init()
}

func (h *home) Update(msg tea.Msg) (tea.Model, tea.Cmd) {
	log.Printf("home: msg: %T | %v", msg, msg)
	switch msg := msg.(type) {
	case tea.WindowSizeMsg:
		h.width = msg.Width
		h.height = msg.Height

		h.accounts.SetWidth(percent(h.width, 20))
		h.balance.SetWidth(percent(h.width, 30))
		h.register.SetWidth(percent(h.width, 60))

		col, row := h.accountsData(h.accounts.Width())
		h.accounts.SetColumns(col)
		h.accounts.SetRows(row)

		h.accounts.Focus()
		h.selectedAccount = h.accounts.SelectedRow()[0]
		h.balance.SetColumns(h.balanceColumns(h.balance.Width()))

		h.register.SetHeight(h.height - 11)
		h.balance.SetHeight(h.register.Height() - h.accounts.Height() - 5)
		h.register.SetColumns(h.registerColumns(h.register.Width()))

		fg, cmd := h.filterGroup.Update(msg)
		h.filterGroup = fg.(*filterGroup)
		return h, tea.Sequence(
			h.updateBalanceTableCmd,
			cmd,
		)

	case tea.KeyMsg:
		if h.filterGroup.Focused() {
			if msg.String() == "esc" {
				h.filterGroup.Blur()
				h.accounts.Focus()
				return h, nil
			}
			if msg.String() == "enter" {
				h.filterGroup.Blur()
				h.accounts.Focus()
				return h, h.updateBalanceTableCmd
			}

			fg, cmd := h.filterGroup.Update(msg)
			h.filterGroup = fg.(*filterGroup)
			return h, cmd
		}

		switch msg.String() {
		case "q":
			return h, tea.Quit
		case "f":
			h.accounts.Blur()
			h.balance.Blur()
			h.register.Blur()
			h.filterGroup.Focus()
			return h, nil

		case "j":
			if h.accounts.Focused() {
				h.accounts.MoveDown(1)
				return h, h.updateBalanceTableCmd
			} else if h.balance.Focused() {
				h.balance.MoveDown(1)
				return h, h.updateRegisterTableCmd
			} else if h.register.Focused() {
				h.register.MoveDown(1)
			}
			return h, nil
		case "k":
			if h.accounts.Focused() {
				h.accounts.MoveUp(1)
				return h, h.updateBalanceTableCmd
			} else if h.balance.Focused() {
				h.balance.MoveUp(1)
				return h, h.updateRegisterTableCmd
			} else if h.register.Focused() {
				h.register.MoveUp(1)
			}
			return h, nil

		case "1":
			h.accounts.Focus()
			h.balance.Blur()
			h.register.Blur()
		case "2":
			h.accounts.Blur()
			h.balance.Focus()
			h.register.Blur()
		case "3":
			h.accounts.Blur()
			h.balance.Blur()
			h.register.Focus()
		}

	case updateBalance:
		log.Printf("updating balance with %s %s", msg.account, h.filterGroup.DateStart())
		row := h.balanceData(msg.account)
		h.balance.GotoTop()
		h.balance.SetRows(row)
		return h, h.updateRegisterTableCmd

	case updateRegister:
		log.Printf("updating register with %s", msg.subAccount)
		rows := h.registerData(msg.subAccount)
		h.register.GotoTop()
		h.register.SetRows(rows)
	}

	return h, nil
}

func (h *home) updateBalanceTableCmd() tea.Msg {
	return updateBalance{h.accounts.SelectedRow()[0]}
}

func (h *home) updateRegisterTableCmd() tea.Msg {
	h.selectedSubAccount = "assets"
	if len(h.balance.SelectedRow()) > 0 {
		h.selectedSubAccount = h.balance.SelectedRow()[0]
	}
	return updateRegister{h.selectedSubAccount}
}

func (m *home) View() string {
	s := table.DefaultStyles()
	s.Header = s.Header.
		BorderStyle(lipgloss.NormalBorder()).
		BorderForeground(lipgloss.Color("240")).
		BorderBottom(true).
		Bold(false)
	s.Selected = s.Selected.
		Foreground(lipgloss.Color("229")).
		Background(lipgloss.Color("60")).
		Bold(false)

	withSelected := table.DefaultStyles()
	withSelected.Header = s.Header
	withSelected.Selected = withSelected.Selected.
		Foreground(lipgloss.Color("229")).
		Background(lipgloss.Color("57")).
		Bold(false)

	tblStyleActive := lipgloss.NewStyle().Border(lipgloss.RoundedBorder()).BorderForeground(lipgloss.Color("White"))
	tblStyleInactive := lipgloss.NewStyle().Border(lipgloss.RoundedBorder()).BorderForeground(lipgloss.Color("240"))
	titleStyleInactive := lipgloss.NewStyle().Padding(0, 1).Foreground(lipgloss.Color("#AAAAAA")).Bold(true)
	titleStyleActive := lipgloss.NewStyle().Padding(0, 1).Foreground(lipgloss.Color("White")).Bold(true)

	var (
		accTableStyle = tblStyleInactive
		balTableStyle = tblStyleInactive
		regTableStyle = tblStyleInactive

		accTitleStyle = titleStyleInactive
		balTitleStyle = titleStyleInactive
		recTitleStyle = titleStyleInactive
	)

	m.accounts.SetStyles(s)
	m.balance.SetStyles(s)
	m.register.SetStyles(s)

	if m.accounts.Focused() {
		m.accounts.SetStyles(withSelected)
		accTableStyle = tblStyleActive
		accTitleStyle = titleStyleActive
	}

	if m.balance.Focused() {
		m.balance.SetStyles(withSelected)
		balTableStyle = tblStyleActive
		balTitleStyle = titleStyleActive
	}

	if m.register.Focused() {
		m.register.SetStyles(withSelected)
		regTableStyle = tblStyleActive
		recTitleStyle = titleStyleActive
	}

	left := lipgloss.JoinVertical(
		lipgloss.Left,
		accTitleStyle.Render("(1) Account Types"),
		accTableStyle.Render(m.accounts.View()),
		balTitleStyle.Render("(2) Balances"),
		balTableStyle.Render(m.balance.View()),
	)

	recordsTitle := lipgloss.JoinHorizontal(
		lipgloss.Top,
		recTitleStyle.Render(fmt.Sprintf("(3) Records (%s)", m.selectedSubAccount)),
	)
	right := lipgloss.JoinVertical(
		lipgloss.Left,
		recordsTitle,
		regTableStyle.Render(m.register.View()),
	)

	content := lipgloss.JoinVertical(
		lipgloss.Left,
		m.filterGroup.View(),
		lipgloss.JoinHorizontal(
			lipgloss.Top,
			left,
			right,
		),
	)
	return content
}

func percent(number, percentage int) int {
	return (percentage * number) / 100
}

func (h *home) registerColumns(width int) []table.Column {
	return []table.Column{
		{Title: "date", Width: percent(width, 10)},
		{Title: "description", Width: percent(width, 45)},
		{Title: "account", Width: percent(width, 25)},
		{Title: "amount", Width: percent(width, 20)},
	}
}

func (h *home) registerData(account string) []table.Row {
	filter := interfaces.Filter{
		Account:     account,
		DateStart:   h.filterGroup.DateStart(),
		DateEnd:     h.filterGroup.DateEnd(),
		Description: h.filterGroup.Description(),
	}
	registerData, err := h.dataProvider.Records(filter)
	if err != nil {
		panic(err)
	}

	data := registerData[1:]
	rows := make([]table.Row, 0, len(data))
	for i := 0; i < len(data); i++ {
		rows = append(rows, data[len(data)-i-1])
	}

	return rows
}

func (h *home) accountsData(width int) ([]table.Column, []table.Row) {
	data := []table.Row{{"assets"}, {"equity"}, {"expenses"}, {"revenue|income"}, {"liabilities"}}
	cols := []table.Column{{Title: "accounts", Width: width}}
	return cols, data
	// balanceData, err := h.dataProvider.AccountBalances()
	// if err != nil {
	// 	panic(err)
	// }
	//
	// header := balanceData[0]
	// data := balanceData[1:]
	// cols := []table.Column{
	// 	{Title: header[0], Width: percent(width, 25)},
	// 	{Title: header[1], Width: percent(width, 20)},
	// 	{Title: header[2], Width: percent(width, 55)},
	// }
	//
	// rows := make([]table.Row, 0, len(data))
	// for _, row := range data {
	// 	rows = append(rows, row)
	// }
	//
	// return cols, rows
}

var accountToAccountType = map[string]string{
	"assets":         "type:a",
	"equity":         "type:e",
	"expenses":       "type:x",
	"revenue|income": "type:r",
	"liabilities":    "type:l",
}

func (h *home) balanceColumns(width int) []table.Column {
	return []table.Column{
		{Title: "account", Width: percent(width, 65)},
		{Title: "commodity", Width: percent(width, 10)},
		{Title: "balance", Width: percent(width, 25)},
	}
}

func (h *home) balanceData(accountName string) []table.Row {
	filter := interfaces.Filter{
		AccountType: accountToAccountType[accountName],
		Account:     h.filterGroup.AccountName(),
		DateStart:   h.filterGroup.DateStart(),
		DateEnd:     h.filterGroup.DateEnd(),
	}
	balanceData, err := h.dataProvider.SubAccountBalances(filter)
	if err != nil {
		panic(err)
	}

	data := balanceData[1:]
	rows := make([]table.Row, 0, len(data))
	for _, row := range data {
		rows = append(rows, row)
	}

	return rows
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
