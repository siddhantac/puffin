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
}

func newHome(dataProvider interfaces.DataProvider, cmdRunner *cmdRunner) *home {
	regTbl := newCustomTable("(3) register")
	regTbl.name = "register"
	regTbl.SetHeight(20)

	col, row := accountsData(20)
	accTbl := newCustomTable("(1) accounts")
	accTbl.name = "accounts"
	accTbl.SetReady(true)
	accTbl.Focus()
	accTbl.SetHeight(6)
	accTbl.SetColumns(col)
	accTbl.SetRows(row)

	balTbl := newCustomTable("(2) balance")
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
	}
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
		h.queryBalanceTableCmd,
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

		h.accounts.SetWidth(percent(h.width, 20))
		h.balance.SetWidth(percent(h.width, 30))
		h.register.SetWidth(percent(h.width, 60))

		col, row := accountsData(h.accounts.Width())
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
		return h, h.queryBalanceTableCmd

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

	case queryBalance:
		h.balance.SetReady(false)
		h.register.SetReady(false)
		f := func() tea.Msg {
			rows := h.balanceData(msg.account)
			return updateBalance{rows}
		}
		h.cmdRunner.Run(f)
		return h, nil

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
		return h, nil

	case updateRegister:
		h.register.SetReady(true)
		h.register.SetRows(msg.rows)
		return h, nil

	case clearRegister:
		h.register.SetTitleModifier("")
		h.register.SetRows(nil)
		h.register.SetReady(true)
		return h, nil

	default:
		var cmd1, cmd2 tea.Cmd
		h.balance, cmd1 = h.balance.Update(msg)
		h.register, cmd2 = h.register.Update(msg)
		return h, tea.Batch(cmd1, cmd2)
	}

	return h, nil
}

func (h *home) queryBalanceTableCmd() tea.Msg {
	return queryBalance{h.accounts.SelectedRow()[0]}
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

func accountsData(width int) ([]table.Column, []table.Row) {
	data := []table.Row{{"assets"}, {"equity"}, {"expenses"}, {"revenue|income"}, {"liabilities"}}
	cols := []table.Column{{Title: "accounts", Width: width}}
	return cols, data
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

	data := balanceData[1:]
	rows := make([]table.Row, 0, len(data))
	for _, row := range data {
		rows = append(rows, row)
	}

	return rows
}
