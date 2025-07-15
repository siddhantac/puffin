package ui

import (
	"fmt"
	"log"
	"strconv"

	"github.com/siddhantac/puffin/ui/v2/interfaces"

	"github.com/charmbracelet/bubbles/spinner"
	"github.com/charmbracelet/bubbles/table"
	tea "github.com/charmbracelet/bubbletea"
	"github.com/charmbracelet/lipgloss"
)

type home struct {
	height, width       int
	accounts            table.Model
	filterGroup         *filterGroup
	displayOptionsGroup *displayOptionsGroup
	cmdRunner           *cmdRunner

	selectedAccount    string
	selectedSubAccount string
	dataProvider       interfaces.DataProvider

	register      table.Model
	spinner       spinner.Model
	registerReady bool

	balance      table.Model
	balanceReady bool
	accounts2    *tabList
}

func newHome(dataProvider interfaces.DataProvider, cmdRunner *cmdRunner) *home {
	regTbl := table.New(
		table.WithHeight(20),
	)

	col, row := accountsData(20)
	accTbl := table.New(
		table.WithFocused(true),
		table.WithHeight(6),
		table.WithColumns(col),
		table.WithRows(row),
	)

	balTbl := table.New(
		table.WithHeight(6),
	)

	accounts2 := NewTabList([]*tab{
		{name: "assets"},
		{name: "expenses"},
		{name: "equity"},
		{name: "liabilities"},
		{name: "revenue"},
	})

	return &home{
		register:            regTbl,
		accounts:            accTbl,
		balance:             balTbl,
		dataProvider:        dataProvider,
		filterGroup:         newFilterGroupHome(),
		displayOptionsGroup: newDisplayOptionsGroupHome(3, interfaces.ByAccount),
		cmdRunner:           cmdRunner,
		spinner:             newSpinner(),
		accounts2:           accounts2,
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
		h.spinner.Tick,
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

	case spinner.TickMsg:
		var cmd tea.Cmd
		h.spinner, cmd = h.spinner.Update(msg)
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
		// case "1":
		// 	h.accounts.Focus()
		// 	h.balance.Blur()
		// 	h.register.Blur()
		// case "2":
		// 	h.accounts.Blur()
		// 	h.balance.Focus()
		// 	h.register.Blur()
		// case "3":
		// 	h.accounts.Blur()
		// 	h.balance.Blur()
		// 	h.register.Focus()

		case "1", "2", "3", "4", "5":
			index, err := strconv.Atoi(msg.String())
			if err != nil {
				panic(err)
			}
			return h, tea.Batch(queryBalanceTableCmd1(h.accounts2.Tab(index).name))
		case "right":
			acc := h.accounts2.Next().name
			return h, tea.Batch(queryBalanceTableCmd1(acc))
		case "left":
			acc := h.accounts2.Previous().name
			return h, tea.Batch(queryBalanceTableCmd1(acc))

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
		h.balanceReady = false
		h.registerReady = false
		f := func() tea.Msg {
			rows := h.balanceData(msg.account)
			return updateBalance{rows}
		}
		h.cmdRunner.Run(f)
		return h, nil

	case updateBalance:
		h.balanceReady = true
		h.balance.SetRows(msg.rows)
		h.balance.SetCursor(0)
		return h, h.queryRegisterTableCmd

	case queryRegister:
		h.registerReady = false
		f := func() tea.Msg {
			rows := h.registerData(msg.subAccount)
			return updateRegister{rows}
		}
		h.cmdRunner.Run(f)
		return h, nil

	case updateRegister:
		h.registerReady = true
		h.register.SetRows(msg.rows)
		return h, nil

	case clearRegister:
		h.register.SetRows(nil)
		return h, nil
	}

	return h, nil
}

func queryBalanceTableCmd1(account string) tea.Cmd {
	return func() tea.Msg {
		return queryBalance{account}
	}
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

	tblStyleUnready := table.DefaultStyles()
	tblStyleUnready.Header.
		BorderStyle(lipgloss.NormalBorder()).
		BorderForeground(lipgloss.Color("240")).
		BorderBottom(true).
		Foreground(lipgloss.Color("#666666"))
	tblStyleUnready.Cell.Foreground(lipgloss.Color("#666666"))

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

	balanceTitleStr := "   (2) Balances"
	if !m.balanceReady {
		balanceTitleStr = fmt.Sprintf("%s (2) Balances", m.spinner.View())
		m.balance.SetStyles(tblStyleUnready)
	}

	left := lipgloss.JoinVertical(
		lipgloss.Left,
		accTitleStyle.Render("(1) Account Types"),
		accTableStyle.Render(m.accounts.View()),
		balTitleStyle.Render(balanceTitleStr),
		balTableStyle.Render(m.balance.View()),
	)

	recordsTitleStr := fmt.Sprintf("   (3) Records (%s)", m.selectedSubAccount)
	if !m.registerReady {
		recordsTitleStr = fmt.Sprintf("%s (3) Records (%s)", m.spinner.View(), m.selectedSubAccount)
		m.register.SetStyles(tblStyleUnready)
	}
	recordsTitle := lipgloss.JoinHorizontal(
		lipgloss.Top,
		recTitleStyle.Render(recordsTitleStr),
	)
	right := lipgloss.JoinVertical(
		lipgloss.Left,
		recordsTitle,
		regTableStyle.Render(m.register.View()),
	)

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
