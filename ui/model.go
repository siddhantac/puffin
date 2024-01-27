package ui

import (
	"fmt"
	"puffin/accounting"
	"puffin/logger"
	"puffin/ui/colorscheme"

	"github.com/charmbracelet/bubbles/key"
	"github.com/charmbracelet/bubbles/spinner"
	tea "github.com/charmbracelet/bubbletea"
	"github.com/charmbracelet/lipgloss"
	hlgo "github.com/siddhantac/hledger"
)

type model struct {
	tabs                 *Tabs
	assetsPager          ContentModel
	expensesPager        ContentModel
	revenuePager         ContentModel
	liabilitiesTable     ContentModel
	registerTable        ContentModel
	incomeStatementPager ContentModel
	balanceSheetPager    ContentModel
	help                 helpModel
	hlcmd                accounting.HledgerCmd
	quitting             bool
	isFormDisplay        bool
	filterGroup          *filterGroup
	spinner              spinner.Model

	searchFilter             accounting.Filter
	periodFilter             accounting.Filter
	acctDepth                accounting.AccountDepthFilter
	isTxnsSortedByMostRecent bool

	isError       string
	width, height int
}

func newModel(hlcmd accounting.HledgerCmd) *model {
	t := &model{
		tabs: newTabs([]string{
			"assets",
			"expenses",
			"revenue",
			"liabilities",
			"income statement",
			"balance sheet",
			"register",
		}),

		assetsPager:          newPager(),
		expensesPager:        newPager(),
		revenuePager:         newPager(),
		liabilitiesTable:     newPager(),
		incomeStatementPager: newPager(),
		balanceSheetPager:    newPager(),
		registerTable:        NewTable([]int{5, 10, 30, 20, 15}),

		help:                     newHelpModel(),
		hlcmd:                    hlcmd,
		quitting:                 false,
		isFormDisplay:            false,
		filterGroup:              newFilterGroup(),
		searchFilter:             accounting.NoFilter{},
		periodFilter:             accounting.NewPeriodFilter().Yearly(),
		acctDepth:                accounting.NewAccountDepthFilter(),
		isTxnsSortedByMostRecent: true,
		width:                    0,
		height:                   0,
		spinner:                  newSpinner(),
	}

	return t
}

func (m *model) Init() tea.Cmd {
	return tea.Batch(
		tea.EnterAltScreen,
		m.spinner.Tick,
	)
}

func (m *model) Update(msg tea.Msg) (tea.Model, tea.Cmd) {
	var cmd tea.Cmd

	switch msg := msg.(type) {

	case accounting.MsgError:
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
		m.assetsPager.Update(msg)
		m.expensesPager.Update(msg)
		m.revenuePager.Update(msg)
		m.liabilitiesTable.Update(msg)
		m.incomeStatementPager.Update(msg)
		m.balanceSheetPager.Update(msg)

		return m, m.refresh()

	case tea.KeyMsg:
		if m.filterGroup.IsFocused() {
			x, y := m.filterGroup.Update(msg)
			cmd = y
			m.filterGroup = x.(*filterGroup)
			return m, cmd
		}

		m.tabs.Update(msg)

		switch {
		case key.Matches(msg, m.help.keys.Help):
			m.help.help.ShowAll = !m.help.help.ShowAll

		case key.Matches(msg, m.help.keys.Refresh): // manual refresh
			return m, m.refresh()
		case key.Matches(msg, m.help.keys.Quit):
			m.quitting = true
			return m, tea.Quit

		case key.Matches(msg, m.help.keys.Search):
			form := newFilterForm(m, searchFilter)
			return form.Update(nil)
		case key.Matches(msg, m.help.keys.Yearly):
			m.periodFilter = accounting.NewPeriodFilter().Yearly()
			return m, m.refresh()
		case key.Matches(msg, m.help.keys.Monthly):
			m.periodFilter = accounting.NewPeriodFilter().Monthly()
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

			// -------_FILTER
		case key.Matches(msg, m.help.keys.Filter):
			m.filterGroup.Focus()
			x, y := m.filterGroup.Update(nil)
			cmd = y
			m.filterGroup = x.(*filterGroup)
		case key.Matches(msg, m.help.keys.Esc):
			m.filterGroup.Blur()
		}
		// only update the active model for key-presses
		// (we don't want other UI elements reacting to keypress
		// when they are not visible)
		activeTable := m.GetActiveTable()
		activeTable.Update(msg)

	case accounting.Filter:
		switch msg := msg.(type) {
		case accounting.DescriptionFilter:
			m.searchFilter = msg
		case accounting.PeriodFilter:
			m.periodFilter = msg
		}
		return m, m.refresh()

	case accounting.IncomeStatementData:
		m.incomeStatementPager.SetContent(string(msg))
	case accounting.BalanceSheetData:
		m.balanceSheetPager.SetContent(string(msg))
	case accounting.AssetsData:
		m.assetsPager.SetContent(string(msg))
	case accounting.ExpensesData:
		m.expensesPager.SetContent(string(msg))
	case accounting.RevenueData:
		m.revenuePager.SetContent(string(msg))
	case accounting.LiabilitiesData:
		m.liabilitiesTable.SetContent(string(msg))
	case spinner.TickMsg:
		m.spinner, cmd = m.spinner.Update(msg)
	case accounting.RegisterData: // set table data when it changes
		m.registerTable.SetContent(msg)

	default:
		m.registerTable.Update(msg)
		m.assetsPager.Update(msg)
		m.expensesPager.Update(msg)
		m.revenuePager.Update(msg)
		m.liabilitiesTable.Update(msg)
		m.incomeStatementPager.Update(msg)
		m.balanceSheetPager.Update(msg)
	}

	return m, cmd
}

func (m *model) search(query string) tea.Cmd {
	// accountFilter := m.filterGroup.AccountFilter()
	// dateFilter := m.filterGroup.DateFilter()
	return tea.Cmd(
		m.hlcmd.Register(hlgo.NewOptions().WithAccount(m.filterGroup.account.Value())),
	// m.hlcmd.register(m.isTxnsSortedByMostRecent,
	// 	accountFilter,
	// 	dateFilter,
	// 	m.searchFilter,
	// ),
	)
}

func (m *model) refresh() tea.Cmd {
	accountFilter := m.filterGroup.AccountFilter()
	dateFilter := m.filterGroup.DateFilter()
	pf := m.periodFilter.(accounting.PeriodFilter)

	opts := hlgo.NewOptions().
		WithAccount(accountFilter.Value()).
		WithStartDate(dateFilter.Value()).
		WithAccountDepth(m.acctDepth.RawValue()).
		WithPeriod(hlgo.PeriodType(pf.RawValue()))

	optsPretty := opts.WithPretty().WithLayout(hlgo.LayoutBare)

	return tea.Batch(
		setPagerLoading,
		m.hlcmd.Register(opts.WithOutputCSV()),
		// m.hlcmd.register(m.isTxnsSortedByMostRecent,
		// 	m.searchFilter,
		m.hlcmd.Assets(optsPretty),
		m.hlcmd.Incomestatement(optsPretty),
		m.hlcmd.Expenses(optsPretty.WithSortAmount()),
		m.hlcmd.Revenue(optsPretty.WithInvertAmount()),
		m.hlcmd.Liabilities(optsPretty),
		m.hlcmd.Balancesheet(optsPretty),
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
	var v string
	if activeTable.IsReady() {
		v = activeTable.View()
	} else {
		v = fmt.Sprintf("\n %s \n\n", m.spinner.View())
	}

	return lipgloss.JoinVertical(
		lipgloss.Left,
		header(),
		lipgloss.JoinHorizontal(
			lipgloss.Top,
			lipgloss.JoinVertical(
				lipgloss.Right,
				m.tabs.View(),
				m.filterGroup.View(),
			),
			activeItemStyle.Render(v),
		),
		m.help.View(),
	)
}

func (m *model) resetFilters() {
	m.filterGroup.Reset()
	m.searchFilter = accounting.NoFilter{}
	m.acctDepth = accounting.NewAccountDepthFilter()
}

func (m *model) GetActiveTable() ContentModel {
	switch m.tabs.CurrentTab() {
	case 0:
		return m.assetsPager
	case 1:
		return m.expensesPager
	case 2:
		return m.revenuePager
	case 3:
		return m.liabilitiesTable
	case 4:
		return m.incomeStatementPager
	case 5:
		return m.balanceSheetPager
	case 6:
		return m.registerTable
	}
	return nil
}

func header() string {
	return lipgloss.NewStyle().
		Bold(true).
		Background(lipgloss.Color(colorscheme.Nord0)).
		Foreground(theme.SecondaryColor).
		MarginTop(1).
		MarginBottom(1).
		PaddingLeft(7).
		PaddingRight(7).
		Render("Puffin")
}
