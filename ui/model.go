package ui

import (
	"fmt"
	"log"

	"github.com/siddhantac/puffin/ui/keys"

	"github.com/charmbracelet/bubbles/key"
	tea "github.com/charmbracelet/bubbletea"
	"github.com/charmbracelet/lipgloss"
	"github.com/mistakenelf/teacup/help"
	"github.com/siddhantac/hledger"
)

type NavigationMode int

const (
	NavTabs NavigationMode = iota
	NavFilters
	NavSettings
	NavGraphs
)

type model struct {
	config         Config
	settings       *settings
	filterGroup    *filterGroup
	graphView      *graphView
	tabs           *Tabs
	help           help.Model
	showHelp       bool
	navigationMode NavigationMode
}

func newModel(config Config) *model {
	m := &model{
		config:      config,
		settings:    newSettings(config),
		filterGroup: newFilterGroup(),
		graphView:   newGraphView(),
		help:        newHelp(),
		showHelp:    false,
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
	// debug logging
	msglog := fmt.Sprintf("root: msg: %T", msg)
	if _, ok := msg.(content); !ok { // don't log financial data
		msglog = fmt.Sprintf("%s, %v", msglog, msg)
	}
	log.Printf(msglog)

	var cmd tea.Cmd

	switch msg := msg.(type) {
	case tea.WindowSizeMsg:
		headerHeight := lipgloss.Height(header())

		mainViewSizeMsg := tea.WindowSizeMsg{
			Height: msg.Height - headerHeight,
			Width:  msg.Width - lipgloss.Width(m.tabs.View()),
		}

		m.updateAllModels(mainViewSizeMsg)
		m.help.SetSize(mainViewSizeMsg.Width, mainViewSizeMsg.Height)

		log.Printf("root: windowSizeMsg: full: h=%d, w=%d. mainView: h=%d, w=%d", msg.Height, msg.Width, mainViewSizeMsg.Height, mainViewSizeMsg.Width)

		return m, m.refresh()

	case tea.KeyMsg:
		// Handle graph view first if it's showing a graph
		if m.graphView.IsShowingGraph() {
			var mod tea.Model
			mod, cmd = m.graphView.Update(msg)
			m.graphView = mod.(*graphView)
			return m, cmd
		}

		if m.filterGroup.IsFocused() {
			cmd = m.filterGroup.Update(msg)
			return m, cmd
		}

		// Handle register filter activation keys first (before global shortcuts)
		if m.isRegisterTable() && (msg.String() == "/" || msg.String() == "ctrl+f") {
			// Let the register table handle filter activation
			activeTable := m.ActiveTab()
			_, cmd = activeTable.Update(msg)
			return m, cmd
		}

		// Handle up/down navigation through the UI sections
		switch {
		case key.Matches(msg, keys.Up):
			return m, m.handleUpNavigation()
		case key.Matches(msg, keys.Down):
			return m, m.handleDownNavigation()
		case key.Matches(msg, keys.Help):
			m.showHelp = !m.showHelp

		case key.Matches(msg, keys.Refresh): // manual refresh
			// Skip refresh if register filter is focused
			if m.isRegisterFilterFocused() {
				break // Don't handle refresh, let the active table handle it
			}
			return m, m.refresh()
		case key.Matches(msg, keys.Quit):
			return m, tea.Quit

		case key.Matches(msg, keys.ResetFilters):
			// Skip reset filters if register filter is focused  
			if m.isRegisterFilterFocused() {
				break // Don't handle reset, let the active table handle it
			}
			m.resetFilters()
			return m, m.refresh()

		case
			key.Matches(msg, keys.AcctDepthDecr),
			key.Matches(msg, keys.AcctDepthIncr),
			key.Matches(msg, keys.TreeView),
			key.Matches(msg, keys.SortBy),
			key.Matches(msg, keys.Theme),

			key.Matches(
				msg,
				keys.Weekly,
				keys.Monthly,
				keys.Yearly,
				keys.Quarterly,
			):

			// Skip global shortcuts if register filter is focused
			if m.isRegisterFilterFocused() {
				break // Don't handle global shortcuts, let the active table handle it
			}

			var mod tea.Model
			mod, _ = m.settings.Update(msg)
			m.settings = mod.(*settings)
			return m, m.refresh()

		case key.Matches(msg, keys.Filter):
			// Skip main filter if register filter is focused
			if m.isRegisterFilterFocused() {
				break // Don't handle main filter, let the active table handle it
			}
			m.filterGroup.Focus()
			return m, m.filterGroup.Update(nil)
		case key.Matches(msg, keys.Esc):
			m.filterGroup.Blur()
			return m, nil
		}

		// Handle navigation-specific updates based on current mode
		switch m.navigationMode {
		case NavGraphs:
			var graphMod tea.Model
			graphMod, cmd = m.graphView.Update(msg)
			m.graphView = graphMod.(*graphView)
			if cmd != nil {
				return m, cmd
			}
		default:
			// For other navigation modes, let tabs handle navigation  
			m.tabs.Update(msg)
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
	if m.showHelp {
		log.Printf("root: showing help view")
		return lipgloss.JoinVertical(
			lipgloss.Left,
			header(),
			m.help.View(),
		)
	}

	log.Printf("root: showing main view")

	reportSectionTitleStyle := sectionTitleStyle.Copy().MarginBottom(1)
	if !m.filterGroup.IsFocused() {
		reportSectionTitleStyle = reportSectionTitleStyle.
			Background(theme.PrimaryBackground).
			Bold(true)
	}

	// Add third horizontal separator
	thirdSeparatorStyle := lipgloss.NewStyle().
		Foreground(theme.PrimaryForeground).
		MarginTop(1).
		MarginBottom(1).
		MarginLeft(1).
		MarginRight(1).
		BorderStyle(lipgloss.NormalBorder()).
		BorderForeground(theme.PrimaryForeground).
		BorderBottom(true)
	thirdSeparator := thirdSeparatorStyle.Render("")

	// Determine what to show in the right panel
	var rightPanelContent string
	if m.graphView.IsShowingGraph() {
		// Show graph in right panel
		rightPanelContent = m.graphView.RenderGraph()
	} else {
		// Show normal tab content
		rightPanelContent = m.ActiveTab().View()
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
				thirdSeparator,
				m.graphView.View(),
			),
			activeItemStyle.Render(rightPanelContent),
		),
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
			// Check if this is a mainView containing a Table with register filter
			if mainView, ok := t.item.(*mainView); ok {
				if table, ok := mainView.ContentModel.(*Table); ok {
					registerFilter := table.GetRegisterFilter()
					if registerFilter != "" {
						// Use register filter instead of global description filter
						opts = hledger.NewOptions().
							WithAccount(m.filterGroup.account.Value()).
							WithStartDate(m.filterGroup.startDate.Value()).
							WithEndDate(m.filterGroup.endDate.Value()).
							WithAccountDepth(m.settings.accountDepth).
							WithDescription(registerFilter).
							WithOutputCSV(true)
					} else {
						opts = registerOpts
					}
				} else {
					opts = registerOpts
				}
			} else {
				opts = registerOpts
			}
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

// isRegisterTable checks if the active tab is a register table
func (m *model) isRegisterTable() bool {
	activeTab := m.ActiveTab()
	if mainView, ok := activeTab.(*mainView); ok {
		if table, ok := mainView.ContentModel.(*Table); ok {
			return table.Type() == cmdRegister
		}
	}
	return false
}

// isRegisterFilterFocused checks if the active tab is a register table with a focused filter
func (m *model) isRegisterFilterFocused() bool {
	activeTab := m.ActiveTab()
	if mainView, ok := activeTab.(*mainView); ok {
		if table, ok := mainView.ContentModel.(*Table); ok {
			if table.Type() == cmdRegister {
				// Check if table has a focused filter (we'll add a method for this)
				return table.IsFilterFocused()
			}
		}
	}
	return false
}

// handleDownNavigation manages the flow: tabs → filters → settings → graphs → back to tabs
func (m *model) handleDownNavigation() tea.Cmd {
	switch m.navigationMode {
	case NavTabs:
		// Check if we're at the last tab (usually accounts/register)
		if m.tabs.selectedTab == len(m.tabs.tabList)-1 {
			// Move to graphs and automatically show graph one
			m.navigationMode = NavGraphs
			m.graphView.selectedGraph = 0
			m.graphView.currentGraph = GraphLinear
			m.graphView.showGraph = true
			return nil
		} else {
			// Normal tab navigation
			m.tabs.incrementSelection()
			return nil
		}
	case NavFilters:
		m.navigationMode = NavSettings
		return nil
	case NavSettings:
		m.navigationMode = NavGraphs
		return nil
	case NavGraphs:
		// Always move to next graph or back to register
		if m.graphView.selectedGraph < len(m.graphView.graphs)-1 {
			m.graphView.selectedGraph++
			// Auto-show the next graph
			m.graphView.currentGraph = m.graphView.graphs[m.graphView.selectedGraph].typ
			m.graphView.showGraph = true
		} else {
			// Go back to register tab and hide graph
			m.navigationMode = NavTabs
			m.tabs.selectedTab = len(m.tabs.tabList) - 1 // Set to register
			m.graphView.showGraph = false
		}
		return nil
	}
	return nil
}

// handleUpNavigation manages the reverse flow: graphs → settings → filters → tabs
func (m *model) handleUpNavigation() tea.Cmd {
	switch m.navigationMode {
	case NavGraphs:
		// Always move to previous graph or back to register
		if m.graphView.selectedGraph > 0 {
			m.graphView.selectedGraph--
			// Auto-show the previous graph
			m.graphView.currentGraph = m.graphView.graphs[m.graphView.selectedGraph].typ
			m.graphView.showGraph = true
		} else {
			// Go back to register tab (last tab) and hide graph
			m.navigationMode = NavTabs
			m.tabs.selectedTab = len(m.tabs.tabList) - 1
			m.graphView.showGraph = false
		}
		return nil
	case NavSettings:
		m.navigationMode = NavFilters
		return nil
	case NavFilters:
		m.navigationMode = NavTabs
		return nil
	case NavTabs:
		// Check if we're at the first tab
		if m.tabs.selectedTab == 0 {
			// Move to graphs from first tab
			m.navigationMode = NavGraphs
			m.graphView.selectedGraph = len(m.graphView.graphs) - 1 // Start at "three"
			m.graphView.currentGraph = GraphCubic
			m.graphView.showGraph = true
			return nil
		} else {
			// Normal tab navigation
			m.tabs.decrementSelection()
			return nil
		}
	}
	return nil
}
