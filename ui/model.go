package ui

import (
	"puffin/hledger"
	"puffin/logger"
	"puffin/ui/styles"
	"puffin/ui/tabs"

	"github.com/charmbracelet/bubbles/key"
	tea "github.com/charmbracelet/bubbletea"
	"github.com/charmbracelet/lipgloss"
)

var isYear bool

type model struct {
	tabs                 *tabs.Tabs
	assetsTable          *TableWrapper
	expensesTable        *TableWrapper
	revenueTable         *TableWrapper
	liabilitiesTable     *TableWrapper
	registerTable        *TableWrapper
	incomeStatementTable *TableWrapper
	help                 helpModel
	filterPanel          *filterPanel
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
		tabs: tabs.New([]string{
			"assets",
			"expenses",
			"revenue",
			"liabilities",
			"income statement",
			"register",
		}),
		assetsTable:          NewTableWrapper(newAssetsTable()),
		expensesTable:        NewTableWrapper(newExpensesTable()),
		revenueTable:         NewTableWrapper(newRevenueTable()),
		liabilitiesTable:     NewTableWrapper(newLiabilitiesTable()),
		registerTable:        NewTableWrapper(newRegisterTable()),
		incomeStatementTable: NewTableWrapper(newIncomeStatementTable()),
		help:                 newHelpModel(),
		filterPanel:          newFilterPanel(),
		hlcmd:                NewHledgerCmd(hl),
		quitting:             false,
		isFormDisplay:        false,
		// activeRegisterDateFilter: hledger.NewDateFilter().UpToToday(),
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
	var cmds []tea.Cmd
	var cmd tea.Cmd

	_, cmd = m.tabs.Update(msg)
	cmds = append(cmds, cmd)

	switch msg := msg.(type) {

	case msgError:
		if m.isError == "" {
			m.isError = msg.Error()
			logger.Logf("received error: %v", msg)
		}
		return m, nil

	case tea.WindowSizeMsg:
		m.help.help.Width = msg.Width
		m.width = msg.Width
		m.height = msg.Height
		msg.Height = msg.Height - styles.FilterPanelStyle.GetVerticalBorderSize()

		// update all models/tables
		m.registerTable.Update(msg)
		m.assetsTable.Update(msg)
		m.expensesTable.Update(msg)
		m.revenueTable.Update(msg)
		m.liabilitiesTable.Update(msg)
		m.incomeStatementTable.Update(msg)

		return m, m.refresh()

	case tea.KeyMsg:
		if m.filterPanel.IsFocused() {
			var fp tea.Model
			fp, cmd = m.filterPanel.Update(msg)
			m.filterPanel = fp.(*filterPanel)
			return m, cmd
		}

		switch {
		case key.Matches(msg, m.help.keys.Filter):
			m.filterPanel.Focus()
			return m, nil

		case key.Matches(msg, m.help.keys.Help):
			m.help.help.ShowAll = !m.help.help.ShowAll

		case key.Matches(msg, m.help.keys.Refresh): // manual refresh
			return m, m.refresh()
		case key.Matches(msg, m.help.keys.Quit):
			m.quitting = true
			return m, tea.Quit

		// case key.Matches(msg, m.help.keys.AccountFilter):
		// 	form := newFilterForm(m, accountFilter)
		// 	return form.Update(nil)
		// case key.Matches(msg, m.help.keys.DateFilter):
		// 	form := newFilterForm(m, dateFilter)
		// 	return form.Update(nil)
		// case key.Matches(msg, m.help.keys.Search):
		// 	form := newFilterForm(m, searchFilter)
		// 	return form.Update(nil)
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
		// case hledger.AccountFilter:
		// 	m.activeAccountFilter = msg
		// case hledger.DateFilter:
		// 	m.activeBalanceDateFilter = msg
		// m.activeRegisterDateFilter = msg
		case hledger.DescriptionFilter:
			m.searchFilter = msg
		case hledger.PeriodFilter:
			m.periodFilter = msg
		}
		return m, m.refresh()

	case Refresh:
		return m, m.refresh()

	default:
		m.registerTable.Update(msg)
		m.assetsTable.Update(msg)
		m.expensesTable.Update(msg)
		m.revenueTable.Update(msg)
		m.liabilitiesTable.Update(msg)
		m.incomeStatementTable.Update(msg)
	}

	return m, tea.Batch(cmds...)
}

func (m *model) search(query string) tea.Cmd {
	filters := m.filterPanel.Filter()
	filters = append(filters, m.searchFilter)
	return tea.Cmd(
		m.hlcmd.register(m.isTxnsSortedByMostRecent,
			// m.activeAccountFilter,
			filters...,
		// m.searchFilter,
		),
	)
}

func (m *model) refresh() tea.Cmd {
	filters := m.filterPanel.Filter()
	filters = append(filters, m.searchFilter, m.acctDepth)
	return tea.Batch(
		m.hlcmd.register(m.isTxnsSortedByMostRecent,
			// m.activeAccountFilter,
			// m.activeRegisterDateFilter,
			filters...,
		),
		m.hlcmd.assets(
			m.activeAccountFilter,
			m.activeBalanceDateFilter,
			m.acctDepth,
			m.periodFilter,
		),
		m.hlcmd.expenses(
			m.activeAccountFilter,
			m.activeBalanceDateFilter,
			m.acctDepth,
			m.periodFilter,
		),
		m.hlcmd.revenue(
			m.activeAccountFilter,
			m.activeBalanceDateFilter,
			m.acctDepth,
			m.periodFilter,
		),
		m.hlcmd.liabilities(
			m.activeAccountFilter,
			m.activeBalanceDateFilter,
			m.acctDepth,
			m.periodFilter,
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
			styles.ContainerStyle.Render(m.tabs.View()),
			m.isError,
			styles.ContainerStyle.Render(m.help.View()),
		)
	}

	activeTable := m.GetActiveTable()

	return lipgloss.JoinVertical(
		lipgloss.Top,
		styles.ContainerStyle.Render(m.filterPanel.View()),
		styles.ContainerStyle.Render(m.tabs.View()),
		styles.ContainerStyle.Render(styles.ActiveTableStyle.Render(activeTable.View())),
		styles.ContainerStyle.Render(m.help.View()),
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
		return m.assetsTable
	case 1:
		return m.expensesTable
	case 2:
		return m.revenueTable
	case 3:
		return m.liabilitiesTable
	case 4:
		return m.incomeStatementTable
	case 5:
		return m.registerTable
	}
	return nil
}
