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
	config        Config
	tabs          *Tabs
	registerTable ContentModel
	accountsPager ContentModel
	genericPagers []*genericPager
	help          helpModel
	hlcmd         accounting.HledgerCmd
	quitting      bool
	isFormDisplay bool
	filterGroup   *filterGroup
	settings      *settings

	isTxnsSortedByMostRecent bool

	msgError      *accounting.MsgError
	width, height int
}

func newModel(hlcmd accounting.HledgerCmd, config Config) *model {
	m := &model{
		config:        config,
		accountsPager: newPager("accounts"),
		genericPagers: make([]*genericPager, 0),
		registerTable: newTable([]int{5, 10, 30, 20, 15}),
		settings:      newSettings(config),

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

	tabs := []TabItem{}

	for i, r := range config.Reports {
		gp := newGenericPager(i, r.Name, r.Locked, runCommand(r.Cmd))
		m.genericPagers = append(m.genericPagers, gp)
		tabs = append(tabs, TabItem{name: r.Name, item: gp})
	}

	tabs = append(tabs,
		TabItem{name: "register", item: m.registerTable},
		TabItem{name: "accounts", item: m.accountsPager},
	)

	m.tabs = newTabs(tabs)
	return m
}

func (m *model) Init() tea.Cmd {
	batchCmds := []tea.Cmd{}
	for _, p := range m.genericPagers {
		batchCmds = append(batchCmds, p.Init())
	}
	batchCmds = append(batchCmds, tea.EnterAltScreen, m.registerTable.Init(), m.accountsPager.Init())

	return tea.Batch(
		batchCmds...,
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
		WithDescription(m.filterGroup.description.Value()).
		WithOutputCSV(true)

	opts := hledger.NewOptions().
		WithAccount(m.filterGroup.account.Value()).
		WithStartDate(m.filterGroup.startDate.Value()).
		WithEndDate(m.filterGroup.endDate.Value()).
		WithAccountDepth(m.settings.accountDepth).
		WithAverage(true).
		WithPeriod(hledger.PeriodType(m.settings.period.periodType)).
		WithTree(m.settings.treeView).
		WithPretty(true).
		WithLayout(hledger.LayoutBare).
		WithAccountDrop(1).
		WithSortAmount(m.settings.toggleSort)

	accountOpts := hledger.NewOptions().
		WithAccount(m.filterGroup.account.Value()).
		WithAccountDepth(m.settings.accountDepth).
		WithTree(m.settings.treeView)

	batchCmds := []tea.Cmd{
		m.hlcmd.Register(registerOpts),
		m.hlcmd.Accounts(accountOpts),
	}

	for _, p := range m.genericPagers {
		if p.locked {
			batchCmds = append(batchCmds, p.Run(hledger.NewOptions()))
		}
		batchCmds = append(batchCmds, p.Run(opts))
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
