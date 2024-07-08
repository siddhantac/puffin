package ui

import (
	"fmt"
	"log"
	"puffin/accounting"
	"puffin/ui/colorscheme"

	"github.com/charmbracelet/bubbles/key"
	tea "github.com/charmbracelet/bubbletea"
	"github.com/charmbracelet/lipgloss"
	"github.com/siddhantac/hledger"
)

type model struct {
	config               Config
	tabs                 *Tabs
	assetsPager          ContentModel
	expensesPager        ContentModel
	revenuePager         ContentModel
	liabilitiesPager     ContentModel
	registerTable        ContentModel
	incomeStatementPager ContentModel
	balanceSheetPager    ContentModel
	accountsPager        ContentModel
	genericPagers        []*genericPager
	help                 helpModel
	hlcmd                accounting.HledgerCmd
	quitting             bool
	isFormDisplay        bool
	filterGroup          *filterGroup
	settings             *settings

	isTxnsSortedByMostRecent bool

	msgError      *accounting.MsgError
	width, height int
}

func newModel(hlcmd accounting.HledgerCmd, config Config) *model {
	m := &model{
		config:               config,
		assetsPager:          newPager("assets"),
		expensesPager:        newPager("expenses"),
		revenuePager:         newPager("revenue"),
		liabilitiesPager:     newPager("liabilities"),
		incomeStatementPager: newPager("incomeStatement"),
		balanceSheetPager:    newPager("balanceSheet"),
		accountsPager:        newPager("accounts"),
		genericPagers:        make([]*genericPager, 0),
		registerTable:        newTable([]int{5, 10, 30, 20, 15}),
		settings:             newSettings(config),

		help:                     newHelpModel(),
		hlcmd:                    hlcmd,
		quitting:                 false,
		isFormDisplay:            false,
		filterGroup:              newFilterGroup(),
		isTxnsSortedByMostRecent: true,
		width:                    0,
		height:                   0,
	}

	m.filterGroup.setStartDate(m.config.StartDate)
	m.filterGroup.setEndDate(m.config.EndDate)

	tabs := []TabItem{
		{name: "assets", item: m.assetsPager},
		{name: "expenses", item: m.expensesPager},
		{name: "revenue", item: m.revenuePager},
		{name: "liabilities", item: m.liabilitiesPager},
		{name: "income statement", item: m.incomeStatementPager},
		{name: "balance sheet", item: m.balanceSheetPager},
		{name: "register", item: m.registerTable},
		{name: "accounts", item: m.accountsPager},
	}

	for i, r := range config.Reports {
		gp := newGenericPager(i, r.Name, runCommand(r.Cmd))
		m.genericPagers = append(m.genericPagers, gp)
		tabs = append(tabs, TabItem{name: r.Name, item: gp})
	}

	m.tabs = newTabs(tabs)
	return m
}

func (m *model) Init() tea.Cmd {
	return tea.Batch(
		tea.EnterAltScreen,
		m.incomeStatementPager.Init(),
		m.registerTable.Init(),
		m.assetsPager.Init(),
		m.expensesPager.Init(),
		m.revenuePager.Init(),
		m.liabilitiesPager.Init(),
		m.balanceSheetPager.Init(),
		m.accountsPager.Init(),
		m.genericPagers[0].Init(),
	)
}

func (m *model) Update(msg tea.Msg) (tea.Model, tea.Cmd) {
	var cmd tea.Cmd

	switch msg := msg.(type) {

	case accounting.MsgError:
		m.msgError = &msg
		log.Printf("received error: %v", msg)
		return m, nil

	case tea.WindowSizeMsg:
		m.help.Width = msg.Width
		m.width = msg.Width
		m.height = msg.Height
		log.Printf("model: WindowSizeMsg: h=%v", msg.Height)

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
			m.help.ShowAll = !m.help.ShowAll

		case key.Matches(msg, m.help.keys.Refresh): // manual refresh
			return m, m.refresh()
		case key.Matches(msg, m.help.keys.Quit):
			m.quitting = true
			return m, tea.Quit

		case key.Matches(msg, m.help.keys.ResetFilters):
			m.resetFilters()
			return m, m.refresh()

		case
			key.Matches(msg, m.help.keys.AcctDepthDecr),
			key.Matches(msg, m.help.keys.AcctDepthIncr),
			key.Matches(msg, m.help.keys.TreeView),
			key.Matches(msg, m.help.keys.SortBy),

			key.Matches(
				msg,
				m.help.keys.Weekly,
				m.help.keys.Monthly,
				m.help.keys.Yearly,
				m.help.keys.Quarterly,
			):

			var mod tea.Model
			mod, cmd = m.settings.Update(msg)
			m.settings = mod.(*settings)
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
		_, cmd = activeTable.Update(msg)

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
	case accounting.AccountsData:
		m.accountsPager.SetContent(string(msg))
	case genericContent:
		for i := range m.genericPagers {
			m.genericPagers[i].SetContent(msg)
		}

	case modelLoading:
		m.setUnreadyAllModels()

	default:
		cmd = m.updateAllModels(msg)
	}

	return m, cmd
}

func (m *model) View() string {
	if m.quitting {
		return ""
	}

	var mainView string

	activeTab := m.ActiveTab()

	if m.msgError != nil {
		msg := fmt.Sprintf("⚠️ Error\n\n\t%s", string(*m.msgError))
		mainView = lipgloss.NewStyle().Foreground(theme.Accent).Render(msg)
	} else {
		mainView = activeTab.View()
	}

	reportSectionTitleStyle := sectionTitleStyle.Copy().MarginBottom(1)
	if !m.filterGroup.IsFocused() {
		reportSectionTitleStyle = reportSectionTitleStyle.
			Background(lipgloss.Color(colorscheme.Nord0)).
			Bold(true)
	}

	return lipgloss.JoinVertical(
		lipgloss.Left,
		header(),
		lipgloss.JoinHorizontal(
			lipgloss.Top,
			lipgloss.JoinVertical(
				lipgloss.Right,
				reportSectionTitleStyle.Render("REPORTS"),
				m.tabs.View(),
				m.filterGroup.View(),
				m.settings.View(),
			),
			activeItemStyle.Render(mainView),
		),
		m.help.View(),
	)
}

func (m *model) setUnreadyAllModels() {
	for _, t := range m.tabs.tabList {
		t.item.SetUnready()
	}
}

func (m *model) updateAllModels(msg tea.Msg) tea.Cmd {
	var cmds []tea.Cmd
	for _, t := range m.tabs.tabList {
		_, cmd := t.item.Update(msg)
		cmds = append(cmds, cmd)
	}
	return tea.Batch(cmds...)
}

func (m *model) refresh() tea.Cmd {
	m.msgError = nil // reset the msgError

	registerOpts := hledger.NewOptions().
		WithAccount(m.filterGroup.account.Value()).
		WithStartDate(m.filterGroup.startDate.Value()).
		WithEndDate(m.filterGroup.endDate.Value()).
		WithAccountDepth(m.settings.accountDepth).
		WithDescription(m.filterGroup.description.Value())

	opts := hledger.NewOptions().
		WithAccount(m.filterGroup.account.Value()).
		WithStartDate(m.filterGroup.startDate.Value()).
		WithEndDate(m.filterGroup.endDate.Value()).
		WithAccountDepth(m.settings.accountDepth).
		WithAverage().
		WithPeriod(hledger.PeriodType(m.settings.period.periodType))

	accountOpts := hledger.NewOptions().
		WithAccount(m.filterGroup.account.Value()).
		WithAccountDepth(m.settings.accountDepth)

	if m.settings.treeView {
		opts = opts.WithTree()
		accountOpts = accountOpts.WithTree()
	}

	optsPretty := opts.WithPretty().WithLayout(hledger.LayoutBare).WithAccountDrop(1)

	if m.settings.toggleSort {
		optsPretty = optsPretty.WithSortAmount()
	}

	batchCmds := []tea.Cmd{
		m.hlcmd.Register(registerOpts.WithOutputCSV()),
		m.hlcmd.Assets(optsPretty),
		m.hlcmd.Incomestatement(optsPretty),
		m.hlcmd.Expenses(optsPretty),
		m.hlcmd.Revenue(optsPretty.WithInvertAmount()),
		m.hlcmd.Liabilities(optsPretty),
		m.hlcmd.Balancesheet(optsPretty),
		m.hlcmd.Accounts(accountOpts),
	}

	for _, p := range m.genericPagers {
		batchCmds = append(batchCmds, p.Run(optsPretty))
	}

	return tea.Sequence(
		setModelLoading,
		tea.Batch(
			batchCmds...,
		),
	)
}

func (m *model) resetFilters() {
	m.filterGroup.Reset()
	m.settings.period.periodType = hledger.PeriodYearly
	m.settings.accountDepth = 2
}

func (m *model) ActiveTab() ContentModel {
	item := m.tabs.CurrentTab().item
	cm := item.(ContentModel)
	return cm
}
