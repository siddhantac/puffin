package ui

import (
	"puffin/hledger"
	"puffin/logger"

	"github.com/charmbracelet/bubbles/key"
	tea "github.com/charmbracelet/bubbletea"
	"github.com/charmbracelet/lipgloss"
)

var isYear bool

type model struct {
	tabs                 *Tabs
	registerTable        *TableWrapper
	balanceTable         *TableWrapper
	incomeStatementTable *TableWrapper
	help                 helpModel
	hlcmd                HledgerCmd
	quitting             bool
	isFormDisplay        bool

	activeRegisterDateFilter hledger.Filter
	activeBalanceDateFilter  hledger.Filter
	activeAccountFilter      hledger.Filter
	searchFilter             hledger.Filter
	periodFilter             hledger.Filter
	acctDepth                hledger.AccountDepthFilter
	isTxnsSortedByMostRecent bool

	isError       string
	width, height int
}

func newModel(hl hledger.Hledger) *model {
	t := &model{
		tabs:                     newTabs(),
		registerTable:            NewTableWrapper(newRegisterTable()),
		balanceTable:             NewTableWrapper(newBalanceTable()),
		incomeStatementTable:     NewTableWrapper(newIncomeStatementTable()),
		help:                     newHelpModel(),
		hlcmd:                    NewHledgerCmd(hl),
		quitting:                 false,
		isFormDisplay:            false,
		activeRegisterDateFilter: hledger.NewDateFilter().UpToToday(),
		activeBalanceDateFilter:  hledger.NewDateFilter().UpToToday(),
		activeAccountFilter:      hledger.NoFilter{},
		searchFilter:             hledger.NoFilter{},
		periodFilter:             hledger.NoFilter{},
		acctDepth:                hledger.NewAccountDepthFilter(),
		isTxnsSortedByMostRecent: true,
		width:                    0,
		height:                   0,
	}

	return t
}

func (m *model) Init() tea.Cmd {
	return tea.Batch(
		tea.EnterAltScreen,
	)
}

func (m *model) Update(msg tea.Msg) (tea.Model, tea.Cmd) {
	m.tabs.Update(msg)

	switch msg := msg.(type) {

	case msgError:
		if m.isError == "" {
			m.isError = msg.Error()
			logger.Logf("received error: %v", msg)
		}
		return m, nil
		// return nil, tea.Quit

	case tea.WindowSizeMsg:
		m.help.help.Width = msg.Width
		m.width = msg.Width
		m.height = msg.Height

		// update all models/tables
		m.registerTable.Update(msg)
		m.balanceTable.Update(msg)
		m.incomeStatementTable.Update(msg)

		return m, m.refresh()

	case tea.KeyMsg:
		switch {
		case key.Matches(msg, m.help.keys.Help):
			m.help.help.ShowAll = !m.help.help.ShowAll

		case key.Matches(msg, m.help.keys.Refresh): // manual refresh
			return m, m.refresh()
		case key.Matches(msg, m.help.keys.Quit):
			m.quitting = true
			return m, tea.Quit

		case key.Matches(msg, m.help.keys.AccountFilter):
			form := newFilterForm(m, accountFilter)
			return form.Update(nil)
		case key.Matches(msg, m.help.keys.DateFilter):
			form := newFilterForm(m, dateFilter)
			return form.Update(nil)
		case key.Matches(msg, m.help.keys.Search):
			form := newFilterForm(m, searchFilter)
			return form.Update(nil)
		case key.Matches(msg, m.help.keys.Yearly):
			m.periodFilter = hledger.NewPeriodFilter().Yearly()
			return m, m.refresh()
		case key.Matches(msg, m.help.keys.Monthly):
			m.periodFilter = hledger.NewPeriodFilter().Monthly()
			return m, m.refresh()
		case key.Matches(msg, m.help.keys.ResetFilters):
			m.resetFilters()
			return m, m.refresh()

		case key.Matches(msg, m.help.keys.AcctDepthDecr):
			m.acctDepth = m.acctDepth.DecreaseDepth()
			return m, m.refresh()
		case key.Matches(msg, m.help.keys.AcctDepthIncr):
			m.acctDepth = m.acctDepth.IncreaseDepth()
			return m, m.refresh()
		}

		// only update the active model for key-presses
		// (we don't want other UI elements reacting to keypress
		// when they are not visible)
		activeTable := m.GetActiveTable()
		activeTable.Update(msg)

	case hledger.Filter:
		switch msg := msg.(type) {
		case hledger.AccountFilter:
			m.activeAccountFilter = msg
		case hledger.DateFilter:
			m.activeBalanceDateFilter = msg
			m.activeRegisterDateFilter = msg
		case hledger.DescriptionFilter:
			m.searchFilter = msg
		case hledger.PeriodFilter:
			m.periodFilter = msg
		}
		return m, m.refresh()

	default:
		m.registerTable.Update(msg)
		m.balanceTable.Update(msg)
		m.incomeStatementTable.Update(msg)
	}

	return m, nil
}

/* func (m *model) Update(msg tea.Msg) (tea.Model, tea.Cmd) {
	m.registerTable.Update(msg)
	m.tabs.Update(msg)

	switch msg := msg.(type) {

	case tea.WindowSizeMsg:
		m.help.help.Width = msg.Width
		m.width = msg.Width
		m.height = msg.Height
		return m, m.refresh()
	case tea.KeyMsg:
		switch {
		// navigation
		case key.Matches(msg, m.help.keys.Up):
			if m.registerTable.Focused() {
				m.registerTable.MoveUp(1)
			} else if m.balanceTable.Focused() {
				m.balanceTable.MoveUp(1)
			}
			return m, nil
		case key.Matches(msg, m.help.keys.Down):
			if m.registerTable.Focused() {
				m.registerTable.MoveDown(1)
			} else if m.balanceTable.Focused() {
				m.balanceTable.MoveDown(1)
			}
			return m, nil
		// case key.Matches(msg, m.help.keys.Switch):
		// 	if m.registerTable.Focused() {
		// 		m.balanceTable.Focus()
		// 		m.registerTable.Blur()
		// 	} else if m.balanceTable.Focused() {
		// 		m.registerTable.Focus()
		// 		m.balanceTable.Blur()
		// 	}
		// 	return m, nil

		// help
		case key.Matches(msg, m.help.keys.Help):
			m.help.help.ShowAll = !m.help.help.ShowAll

			// filters
		case key.Matches(msg, m.help.keys.AccountFilter):
			form := newFilterForm(m, accountFilter)
			return form.Update(nil)
		case key.Matches(msg, m.help.keys.DateFilter):
			form := newFilterForm(m, dateFilter)
			return form.Update(nil)
		case key.Matches(msg, m.help.keys.Search):
			form := newFilterForm(m, searchFilter)
			return form.Update(nil)

		case key.Matches(msg, m.help.keys.Refresh): // manual refresh
			return m, m.refresh()
		case key.Matches(msg, m.help.keys.Quit):
			m.quitting = true
			return m, tea.Quit

		case key.Matches(msg, m.help.keys.SwapSortingByDate):
			m.isTxnsSortedByMostRecent = !m.isTxnsSortedByMostRecent
			return m, m.hlcmd.register(m.isTxnsSortedByMostRecent, m.activeAccountFilter, m.activeRegisterDateFilter)

		}

	case accountsData: // set table data when it changes
		m.balanceTable.SetRows(msg)
	case transactionsData: // set table data when it changes
		m.registerTable.SetRows(msg)

	case hledger.Filter:
		switch msg := msg.(type) {
		case hledger.AccountFilter:
			m.activeAccountFilter = msg
		case hledger.DateFilter:
			m.activeBalanceDateFilter = msg
			m.activeRegisterDateFilter = msg
		case hledger.DescriptionFilter:
			m.searchFilter = msg
		}

		return m, m.refresh()
	}

	return m, nil
} */

func (m *model) search(query string) tea.Cmd {
	return tea.Cmd(
		m.hlcmd.register(m.isTxnsSortedByMostRecent,
			m.activeAccountFilter,
			m.activeRegisterDateFilter,
			m.searchFilter,
		),
	)
}

func (m *model) refresh() tea.Cmd {
	return tea.Batch(
		m.hlcmd.register(m.isTxnsSortedByMostRecent,
			m.activeAccountFilter,
			m.activeRegisterDateFilter,
			m.searchFilter,
			m.acctDepth,
		),
		m.hlcmd.balance(
			m.activeAccountFilter,
			m.activeBalanceDateFilter,
			m.acctDepth,
			m.periodFilter,
		),
		m.hlcmd.incomestatement(
			m.activeBalanceDateFilter,
			m.acctDepth,
			m.periodFilter,
		),
	)
}

func (m *model) View() string {
	if m.quitting {
		return ""
	}

	if m.isError != "" {
		return lipgloss.JoinVertical(
			lipgloss.Top,
			containerStyle.Render(m.tabs.View()),
			m.isError,
			containerStyle.Render(m.help.View()),
		)
	}

	activeTable := m.GetActiveTable()

	return lipgloss.JoinVertical(
		lipgloss.Top,
		containerStyle.Render(m.tabs.View()),
		containerStyle.Render(activeTableStyle.Render(activeTable.View())),
		containerStyle.Render(m.help.View()),
	)
}

func (m *model) resetFilters() {
	m.activeRegisterDateFilter = hledger.NewDateFilter().UpToToday()
	m.activeBalanceDateFilter = hledger.NewDateFilter().UpToToday()
	m.activeAccountFilter = hledger.NoFilter{}
	m.searchFilter = hledger.NoFilter{}
	m.acctDepth = hledger.NewAccountDepthFilter()
}

func (m *model) GetActiveTable() tea.Model {
	switch m.tabs.CurrentTab() {
	case 0:
		return m.registerTable
	case 1:
		return m.registerTable
	case 2:
		return m.balanceTable
	case 3:
		return m.incomeStatementTable
	}
	return nil
}
