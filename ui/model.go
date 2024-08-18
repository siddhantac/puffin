package ui

import (
	"log"
	"puffin/ui/colorscheme"

	"github.com/charmbracelet/bubbles/key"
	tea "github.com/charmbracelet/bubbletea"
	"github.com/charmbracelet/lipgloss"
	"github.com/siddhantac/hledger"
)

type model struct {
	config        Config
	tabs          *Tabs
	help          helpModel
	quitting      bool
	isFormDisplay bool
	filterGroup   *filterGroup
	settings      *settings
	width, height int
}

func newModel(config Config) *model {
	m := &model{
		config:   config,
		settings: newSettings(config),

		help:          newHelpModel(),
		quitting:      false,
		isFormDisplay: false,
		filterGroup:   newFilterGroup(),
		width:         0,
		height:        0,
	}

	m.filterGroup.setStartDate(m.config.StartDate)
	m.filterGroup.setEndDate(m.config.EndDate)

	dataTransformers := []dataTransformer{newAccountTreeView(m.TreeView)}

	tabItems := []TabItem{}

	for i, r := range config.Reports {
		contentModel := detectCommand(i, r, dataTransformers)
		tabItems = append(tabItems, TabItem{name: r.Name, item: contentModel})
	}

	m.tabs = newTabs(tabItems)
	return m
}

func (m *model) Init() tea.Cmd {
	batchCmds := []tea.Cmd{
		tea.EnterAltScreen,
	}
	for _, t := range m.tabs.tabList {
		batchCmds = append(batchCmds, t.item.Init())
	}

	return tea.Batch(
		batchCmds...,
	)
}

func (m *model) Update(msg tea.Msg) (tea.Model, tea.Cmd) {
	var cmd tea.Cmd

	switch msg := msg.(type) {
	case tea.WindowSizeMsg:
		m.help.Width = msg.Width
		m.width = msg.Width
		m.height = msg.Height

		headerHeight := lipgloss.Height(header())
		footerHeight := lipgloss.Height(m.help.View())
		msg.Height = msg.Height - (headerHeight + footerHeight)
		msg.Width = msg.Width - lipgloss.Width(m.tabs.View())

		m.updateAllModels(msg)

		log.Printf("WindowSizeMsg: full: h=%d, w=%d. mainView: h=%d, w=%d", m.height, msg.Width, msg.Height, msg.Width)

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
			mod, _ = m.settings.Update(msg)
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

	case content:
		for i, tab := range m.tabs.tabList {
			if i == msg.id {
				tab.item.SetContent(msg)
			}
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
			activeItemStyle.Render(m.ActiveTab().View()),
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
	registerOpts := hledger.NewOptions().
		WithAccount(m.filterGroup.account.Value()).
		WithStartDate(m.filterGroup.startDate.Value()).
		WithEndDate(m.filterGroup.endDate.Value()).
		WithAccountDepth(m.settings.accountDepth).
		WithDescription(m.filterGroup.description.Value()).
		WithOutputCSV(true)

	generalOpts := hledger.NewOptions().
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

	balanceOpts := hledger.NewOptions().
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
		WithSortAmount(m.settings.toggleSort).
		WithOutputCSV(true)

	accountOpts := hledger.NewOptions().
		WithTree(m.settings.treeView)

	batchCmds := []tea.Cmd{}

	for _, t := range m.tabs.tabList {
		if t.item.Locked() {
			batchCmds = append(batchCmds, t.item.Run(hledger.NewOptions()))
			continue
		}

		var opts hledger.Options
		switch t.item.Type() {
		case cmdBalance:
			opts = balanceOpts
		case cmdRegister:
			opts = registerOpts
		case cmdAccounts:
			opts = accountOpts
		default:
			opts = generalOpts
		}
		batchCmds = append(batchCmds, t.item.Run(opts))
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
	return item
}

func (m *model) TreeView() bool {
	return m.settings.treeView
}
