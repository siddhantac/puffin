package ui

import (
	"fmt"
	"puffin/accounting"
	"puffin/logger"

	"github.com/charmbracelet/bubbles/key"
	"github.com/charmbracelet/bubbles/spinner"
	tea "github.com/charmbracelet/bubbletea"
	"github.com/charmbracelet/lipgloss"
	"github.com/siddhantac/hledger"
)

type model struct {
	tabs                 *Tabs
	assetsPager          ContentModel
	expensesPager        ContentModel
	revenuePager         ContentModel
	liabilitiesPager     ContentModel
	registerTable        ContentModel
	incomeStatementPager ContentModel
	balanceSheetPager    ContentModel
	help                 helpModel
	hlcmd                accounting.HledgerCmd
	quitting             bool
	isFormDisplay        bool
	filterGroup          *filterGroup
	spinner              spinner.Model
	period               *Period

	searchFilter             accounting.Filter
	acctDepth                accounting.AccountDepthFilter
	isTxnsSortedByMostRecent bool

	msgError      *accounting.MsgError
	width, height int
}

func newModel(hlcmd accounting.HledgerCmd) *model {
	t := &model{

		assetsPager:          newPager(),
		expensesPager:        newPager(),
		revenuePager:         newPager(),
		liabilitiesPager:     newPager(),
		incomeStatementPager: newPager(),
		balanceSheetPager:    newPager(),
		registerTable:        newTable([]int{5, 10, 30, 20, 15}),

		help:                     newHelpModel(),
		hlcmd:                    hlcmd,
		quitting:                 false,
		isFormDisplay:            false,
		filterGroup:              newFilterGroup(),
		period:                   newPeriod(),
		searchFilter:             accounting.NoFilter{},
		acctDepth:                accounting.NewAccountDepthFilter(),
		isTxnsSortedByMostRecent: true,
		width:                    0,
		height:                   0,
		spinner:                  newSpinner(),
	}

	t.tabs = newTabs([]TabItem{
		{name: "assets", item: t.assetsPager},
		{name: "expenses", item: t.expensesPager},
		{name: "revenue", item: t.revenuePager},
		{name: "liabilities", item: t.liabilitiesPager},
		{name: "income statement", item: t.incomeStatementPager},
		{name: "balance sheet", item: t.balanceSheetPager},
		{name: "register", item: t.registerTable},
	})
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
		m.msgError = &msg
		logger.Logf("received error: %v", msg)
		return m, nil

	case tea.WindowSizeMsg:
		m.help.help.Width = msg.Width
		m.width = msg.Width
		m.height = msg.Height

		m.updateAllModels(msg)
		return m, m.refresh()

	case tea.KeyMsg:
		if m.filterGroup.IsFocused() {
			cmd = m.filterGroup.Update(msg)
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

		case key.Matches(msg, m.help.keys.Yearly),
			key.Matches(msg, m.help.keys.Monthly),
			key.Matches(msg, m.help.keys.Quarterly):
			m.period.Update(msg)
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

		case key.Matches(msg, m.help.keys.Filter):
			m.filterGroup.Focus()
			return m, m.filterGroup.Update(nil)
		case key.Matches(msg, m.help.keys.Esc):
			m.filterGroup.Blur()
			return m, nil
		}

		// only update the active model for key-presses
		// (we don't want other UI elements reacting to keypress
		// when they are not visible)
		activeTable := m.ActiveTab()
		activeTable.Update(msg)

	case filterApplied:
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
		m.liabilitiesPager.SetContent(string(msg))
	case accounting.RegisterData:
		m.registerTable.SetContent(msg)

	case spinner.TickMsg:
		m.spinner, cmd = m.spinner.Update(msg)
	case modelLoading:
		m.setUnreadyAllModels()

	default:
		m.updateAllModels(msg)
	}

	return m, cmd
}

func (m *model) View() string {
	if m.quitting {
		return ""
	}

	var v string

	activeTab := m.ActiveTab()

	if m.msgError != nil {
		msg := fmt.Sprintf("⚠️ Error\n\n\t%s", string(*m.msgError))
		v = lipgloss.NewStyle().Foreground(theme.Accent).Render(msg)
	} else if activeTab.IsReady() {
		v = activeTab.View()
	} else { // show spinner if tab's data is not ready
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
				m.period.View(),
			),
			activeItemStyle.Render(v),
		),
		m.help.View(),
	)
}

func (m *model) setUnreadyAllModels() {
	m.incomeStatementPager.SetUnready()
	m.registerTable.SetUnready()
	m.assetsPager.SetUnready()
	m.expensesPager.SetUnready()
	m.revenuePager.SetUnready()
	m.liabilitiesPager.SetUnready()
	m.balanceSheetPager.SetUnready()
}

func (m *model) updateAllModels(msg tea.Msg) {
	m.incomeStatementPager.Update(msg)
	m.registerTable.Update(msg)
	m.assetsPager.Update(msg)
	m.expensesPager.Update(msg)
	m.revenuePager.Update(msg)
	m.liabilitiesPager.Update(msg)
	m.balanceSheetPager.Update(msg)
}

func (m *model) refresh() tea.Cmd {
	m.msgError = nil // reset the msgError

	registerOpts := hledger.NewOptions().
		WithAccount(m.filterGroup.account.Value()).
		WithStartDate(m.filterGroup.startDate.Value()).
		WithEndDate(m.filterGroup.endDate.Value()).
		WithAccountDepth(m.acctDepth.RawValue())
	opts := hledger.NewOptions().
		WithAccount(m.filterGroup.account.Value()).
		WithStartDate(m.filterGroup.startDate.Value()).
		WithEndDate(m.filterGroup.endDate.Value()).
		WithAccountDepth(m.acctDepth.RawValue()).
		WithPeriod(hledger.PeriodType(m.period.periodType))

	optsPretty := opts.WithPretty().WithLayout(hledger.LayoutBare).WithAccountDrop(1)

	return tea.Batch(
		setModelLoading,
		m.hlcmd.Register(registerOpts.WithOutputCSV()), // 	m.searchFilter,
		m.hlcmd.Assets(optsPretty),
		m.hlcmd.Incomestatement(optsPretty.WithSortAmount()),
		m.hlcmd.Expenses(optsPretty.WithSortAmount()),
		m.hlcmd.Revenue(optsPretty.WithInvertAmount()),
		m.hlcmd.Liabilities(optsPretty),
		m.hlcmd.Balancesheet(optsPretty.WithSortAmount()),
	)
}

func (m *model) resetFilters() {
	m.filterGroup.Reset()
	m.searchFilter = accounting.NoFilter{}
	m.period.periodType = hledger.PeriodYearly
	m.acctDepth = accounting.NewAccountDepthFilter()
}

func (m *model) ActiveTab() ContentModel {
	item := m.tabs.CurrentTab().item
	cm := item.(ContentModel)
	return cm
}
