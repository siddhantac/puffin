package ui

import (
	"fmt"
	"log"

	"github.com/siddhantac/puffin/ui/v2/interfaces"

	"github.com/charmbracelet/bubbles/table"
	tea "github.com/charmbracelet/bubbletea"
	"github.com/charmbracelet/lipgloss"
)

type home struct {
	height, width       int
	accounts            *customTable
	filterGroup         *filterGroup
	displayOptionsGroup *displayOptionsGroup
	cmdRunner           *cmdRunner

	selectedAccount    string
	selectedSubAccount string
	dataProvider       interfaces.DataProvider

	register *customTable
	balance  *customTable

	tables           []*customTable
	activeTableIndex int
}

func newHome(dataProvider interfaces.DataProvider, cmdRunner *cmdRunner) *home {
	regTbl := newCustomTable("register")
	regTbl.name = "register"
	regTbl.SetHeight(20)

	// col, row := accountsData(20)
	accTbl := newCustomTable("accounts")
	accTbl.name = "accounts"
	accTbl.SetReady(true)
	accTbl.Focus()
	accTbl.SetHeight(6)
	// accTbl.SetColumns(col)
	// accTbl.SetRows(row)

	balTbl := newCustomTable("balance")
	balTbl.SetHeight(6)
	balTbl.name = "balance"

	optionFactory := displayOptionsGroupFactory{}
	filterGroupFactory := filterGroupFactory{}

	return &home{
		register:            regTbl,
		accounts:            accTbl,
		balance:             balTbl,
		dataProvider:        dataProvider,
		filterGroup:         filterGroupFactory.NewGroupHome(),
		displayOptionsGroup: optionFactory.NewHomeGroup(3, interfaces.ByAccount),
		cmdRunner:           cmdRunner,
		tables:              []*customTable{accTbl, balTbl, regTbl},
		activeTableIndex:    0,
	}
}

type queryBalanceAllAccounts struct{}
type updateBalanceAllAccounts struct {
	rows []table.Row
}

type updateBalance struct {
	rows []table.Row
}
type queryBalance struct {
	account string
}

type updateRegister struct {
	rows []table.Row
}
type queryRegister struct {
	subAccount string
}

type clearRegister struct{}

func (h *home) Init() tea.Cmd {
	return tea.Batch(
		h.filterGroup.Init(),
		h.queryBalanceAllAccountsCmd,
		h.accounts.Init(),
		h.balance.Init(),
		h.register.Init(),
	)
}

func (h *home) Update(msg tea.Msg) (tea.Model, tea.Cmd) {
	switch msg := msg.(type) {
	case tea.WindowSizeMsg:
		log.Printf("home: msg: %T | %v", msg, msg)

		h.width = msg.Width
		h.height = msg.Height

		h.accounts.SetWidth(percent(h.width, 40) - 1)
		h.balance.SetWidth(percent(h.width, 40) - 1)
		h.register.SetWidth(percent(h.width, 60) - 1)

		col := accountsData(h.accounts.Width())
		h.accounts.SetColumns(col)
		// h.accounts.SetRows(row)

		h.accounts.Focus()
		// h.selectedAccount = h.accounts.SelectedRow()[0]
		h.balance.SetColumns(h.balanceColumns(h.balance.Width()))

		h.register.SetHeight(h.height - 11)
		h.balance.SetHeight(h.register.Height() - h.accounts.Height() - 5)
		h.register.SetColumns(h.registerColumns(h.register.Width()))

		fg, cmd := h.filterGroup.Update(msg)
		h.filterGroup = fg.(*filterGroup)
		return h, cmd

	case focusFilterMsg:
		log.Printf("home: msg: %T", msg)
		h.accounts.Blur()
		h.balance.Blur()
		h.register.Blur()
		h.filterGroup.Focus()
		return h, nil

	case blurFilterMsg:
		log.Printf("home: msg: %T", msg)
		h.accounts.Focus()
		h.filterGroup.Blur()
		return h, nil

	case refreshDataMsg:
		log.Printf("home: msg: %T", msg)
		h.accounts.Focus()
		h.filterGroup.Blur()
		return h, h.queryBalanceAllAccountsCmd

	case tea.KeyMsg:
		log.Printf("home: msg: %T | %v", msg, msg)
		// TODO: this is similar to capture mode,
		// see if we can reuse the same logic
		// that we are using in ui.go
		if h.filterGroup.Focused() {
			fg, cmd := h.filterGroup.Update(msg)
			h.filterGroup = fg.(*filterGroup)
			return h, cmd
		}

		switch msg.String() {
		case "q":
			return h, tea.Quit

		case "tab":
			for _, t := range h.tables {
				t.Blur()
			}

			n := len(h.tables)
			h.activeTableIndex = (h.activeTableIndex + 1) % n
			h.tables[h.activeTableIndex].Focus()
		case "shift+tab":
			for _, t := range h.tables {
				t.Blur()
			}

			n := len(h.tables)
			h.activeTableIndex = (h.activeTableIndex - 1 + n) % n
			h.tables[h.activeTableIndex].Focus()

		default:
			dg, cmd := h.displayOptionsGroup.Update(msg)
			h.displayOptionsGroup = dg.(*displayOptionsGroup)
			if cmd != nil {
				return h, cmd
			}

			// if accounts table has changed then refresh
			r := h.accounts.Cursor()
			h.accounts, cmd = h.accounts.Update(msg)
			if r != h.accounts.Cursor() {
				return h, tea.Batch(cmd, h.queryBalanceTableCmd)
			}

			// if balance table has changed then refresh
			r = h.balance.Cursor()
			h.balance, cmd = h.balance.Update(msg)
			if r != h.balance.Cursor() {
				return h, tea.Batch(cmd, h.queryRegisterTableCmd)
			}

			h.register, cmd = h.register.Update(msg)
			return h, cmd
		}

	case queryBalanceAllAccounts:
		h.accounts.SetReady(false)
		h.balance.SetReady(false)
		h.register.SetReady(false)
		f := func() tea.Msg {
			rows := h.allAccountsBalanceData()
			return updateBalanceAllAccounts{rows}
		}
		h.cmdRunner.Run(f)
		return h, updateStatusCmd("Loading accounts...")

	case updateBalanceAllAccounts:
		h.accounts.SetReady(true)
		h.accounts.SetRows(msg.rows)
		h.accounts.SetCursor(0)
		return h, h.queryBalanceTableCmd

	case queryBalance:
		h.balance.SetReady(false)
		h.register.SetReady(false)
		f := func() tea.Msg {
			rows := h.balanceData(msg.account)
			return updateBalance{rows}
		}
		h.cmdRunner.Run(f)
		return h, updateStatusCmd("Loading balance...")

	case updateBalance:
		h.balance.SetReady(true)
		h.balance.SetRows(msg.rows)
		h.balance.SetCursor(0)
		return h, h.queryRegisterTableCmd

	case queryRegister:
		h.register.SetReady(false)
		f := func() tea.Msg {
			rows := h.registerData(msg.subAccount)
			h.register.SetTitleModifier(fmt.Sprintf(" (%s)", msg.subAccount))
			return updateRegister{rows}
		}
		h.cmdRunner.Run(f)
		return h, updateStatusCmd("Loading register...")

	case updateRegister:
		h.register.SetReady(true)
		h.register.SetRows(msg.rows)
		return h, clearStatusCmd

	case clearRegister:
		h.register.SetTitleModifier("")
		h.register.SetRows(nil)
		h.register.SetReady(true)
		return h, nil

	default:
		var cmd1, cmd2, cmd3 tea.Cmd
		h.accounts, cmd1 = h.accounts.Update(msg)
		h.balance, cmd2 = h.balance.Update(msg)
		h.register, cmd3 = h.register.Update(msg)
		return h, tea.Batch(cmd1, cmd2, cmd3)
	}

	return h, nil
}

func (h *home) queryBalanceAllAccountsCmd() tea.Msg {
	return queryBalanceAllAccounts{}
}

func (h *home) queryBalanceTableCmd() tea.Msg {
	row := h.accounts.SelectedRow()
	if len(row) == 0 {
		return nil
	}
	return queryBalance{row[0]}
}

func (h *home) queryRegisterTableCmd() tea.Msg {
	h.selectedSubAccount = "assets"
	if len(h.balance.SelectedRow()) > 0 {
		h.selectedSubAccount = h.balance.SelectedRow()[0]
	}

	if h.selectedSubAccount == "Total:" {
		return clearRegister{}
	}
	return queryRegister{h.selectedSubAccount}
}

func (m *home) View() string {
	left := lipgloss.JoinVertical(
		lipgloss.Left,
		m.accounts.View(),
		m.balance.View(),
	)

	right := m.register.View()

	filterView := lipgloss.JoinHorizontal(
		lipgloss.Center,
		m.filterGroup.View(),
		" ",
		lipgloss.NewStyle().
			Border(lipgloss.RoundedBorder(), false, false, false, true).
			BorderForeground(lipgloss.Color("240")).
			Render(divider.View()),
		" ",
		m.displayOptionsGroup.View(),
	)

	content := lipgloss.JoinVertical(
		lipgloss.Left,
		filterView,
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
	w := width - 4*2 // 4 columns * 2 chars cell padding each
	return []table.Column{
		{Title: "date", Width: percent(w, 10)},
		{Title: "description", Width: percent(w, 45)},
		{Title: "account", Width: percent(w, 25)},
		{Title: "amount", Width: percent(w, 20)},
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

	if len(registerData) == 0 {
		return nil
	}
	data := registerData[1:]
	rows := make([]table.Row, 0, len(data))
	for i := 0; i < len(data); i++ {
		rows = append(rows, data[len(data)-i-1])
	}

	return rows
}

func accountsData(width int) []table.Column {
	w := width - 3*2 // 3 columns * 2 chars cell padding each
	return []table.Column{
		{Title: "account", Width: percent(w, 50)},
		{Title: "commodity", Width: percent(w, 20)},
		{Title: "balance", Width: percent(w, 30)},
	}
}

var accountToAccountType = map[string]string{
	"assets":         "type:a",
	"equity":         "type:e",
	"expenses":       "type:x",
	"revenue|income": "type:r",
	"liabilities":    "type:l",
}

func (h *home) balanceColumns(width int) []table.Column {
	w := width - 3*2 // 3 columns * 2 chars cell padding each
	return []table.Column{
		{Title: "account", Width: percent(w, 50)},
		{Title: "commodity", Width: percent(w, 20)},
		{Title: "balance", Width: percent(w, 30)},
	}
}

func (h *home) balanceData(accountName string) []table.Row {
	filter := interfaces.Filter{
		AccountType: accountToAccountType[accountName],
		Account:     h.filterGroup.AccountName(),
		DateStart:   h.filterGroup.DateStart(),
		DateEnd:     h.filterGroup.DateEnd(),
	}

	displayOptions := interfaces.DisplayOptions{
		Depth: h.displayOptionsGroup.DepthValue(),
		Sort:  h.displayOptionsGroup.SortValue(),
	}

	balanceData, err := h.dataProvider.Balance(filter, displayOptions)
	if err != nil {
		panic(err)
	}

	if len(balanceData) <= 1 {
		return nil
	}

	data := balanceData[1 : len(balanceData)-1]
	rows := make([]table.Row, 0, len(data))
	for _, row := range data {
		rows = append(rows, row)
	}

	return rows
}

func (h *home) allAccountsBalanceData() []table.Row {
	filter := interfaces.Filter{
		DateStart: h.filterGroup.DateStart(),
		DateEnd:   h.filterGroup.DateEnd(),
	}

	displayOptions := interfaces.DisplayOptions{
		Depth: 1,
	}

	balanceData, err := h.dataProvider.Balance(filter, displayOptions)
	if err != nil {
		panic(err)
	}

	if len(balanceData) <= 1 {
		return nil
	}

	data := balanceData[1 : len(balanceData)-1]
	rows := make([]table.Row, 0, len(data))
	for _, row := range data {
		rows = append(rows, row)
	}

	return rows
}
