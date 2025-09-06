package ui

import (
	"fmt"
	"log"
	"time"
	"strings"

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
	months         *monthsView
	tabs           *Tabs
	help           help.Model
	showHelp       bool
	navigationMode NavigationMode

	leftPanelWidth int
	contentWidth   int
	contentHeight  int
}

func newModel(config Config) *model {
	m := &model{
		config:         config,
		settings:       newSettings(config),
		filterGroup:    newFilterGroup(),
		graphView:      newGraphView(),
		months:         newMonthsView(),
		help:           newHelp(),
		showHelp:       false,
		leftPanelWidth: 38, // fixed width for left panel
	}

	m.filterGroup.setStartDate(m.config.StartDate)
	m.filterGroup.setEndDate(m.config.EndDate)

	dataTransformers := []dataTransformer{newAccountTreeView(m.TreeView)}

	tabItems := []TabItem{}
	seen := map[string]bool{}

	for i, r := range config.Reports {
		key := strings.ToLower(strings.TrimSpace(r.Name))
		if key == "" { continue }
		if seen[key] { continue }
		seen[key] = true
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
		headerHeight := lipgloss.Height(header()) + 1 // extra line to account for bottom margin/padding

		// Fixed left panel width (v2): 38 columns
		m.leftPanelWidth = 38

		// Compute main view size based on fixed left panel width
		mainViewWidth := msg.Width - m.leftPanelWidth
		if mainViewWidth < 0 {
			mainViewWidth = 0
		}
		mainViewSizeMsg := tea.WindowSizeMsg{
			Height: msg.Height - headerHeight,
			Width:  mainViewWidth,
		}

		m.contentWidth = mainViewSizeMsg.Width
		m.contentHeight = mainViewSizeMsg.Height

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

		// If months selector is active, handle arrow navigation and back keys here
		if m.months.IsActive() {
			switch msg.String() {
			case "left", "up":
				m.months.Prev()
				return m, m.filterRegisterBySelectedMonth()
			case "right", "down":
				m.months.Next()
				return m, m.filterRegisterBySelectedMonth()
			case "esc":
				m.months.Deactivate()
				return m, nil
			}
		}

		// Handle register filter activation keys first (before global shortcuts)
		if m.isRegisterTable() && (msg.String() == "/" || msg.String() == "ctrl+f") {
			// Let the register table handle filter activation
			activeTable := m.ActiveTab()
			_, cmd = activeTable.Update(msg)
			return m, cmd
		}

		// Global shortcuts and toggles
		switch {
		case msg.String() == "m" || msg.String() == "M":
			// Toggle months list; when activating, select January and filter register
			m.months.Toggle()
			if m.months.IsActive() {
				m.months.SelectFirst()
				return m, m.filterRegisterBySelectedMonth()
			}
			return m, nil
		case key.Matches(msg, keys.ShowGraph):
			// Toggle graph view (revenue by default)
			if m.graphView.IsShowingGraph() {
				m.graphView.showGraph = false
			} else {
				m.graphView.selectedGraph = 0
				m.graphView.currentGraph = GraphLinear
				m.graphView.showGraph = true
			}
			return m, nil
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


	// Add third horizontal separator
thirdSeparator := "────────────────"

	// Determine what to show in the right panel
	var rightPanelContent string
	if m.graphView.IsShowingGraph() {
		// Show graph in right panel
		rightPanelContent = m.graphView.RenderGraph()
	} else {
		// Show normal tab content
		rightPanelContent = m.ActiveTab().View()
	}

	leftPanel := lipgloss.JoinVertical(
		lipgloss.Left,
		"",
		"",
		"",
		m.tabs.View(),
		m.filterGroup.View(),
		m.settings.View(),
		thirdSeparator,
		m.graphView.View(),
		// When on register, show months selector under the graphs block
		func() string {
			if m.isRegisterTable() { return m.months.View() }
			return ""
		}(),
	)
	// Style the entire left pane differently when certain tabs are selected
	leftPaneStyle := lipgloss.NewStyle().Width(m.leftPanelWidth)
	if m.shouldHighlightLeftPane() {
		leftPaneStyle = leftPaneStyle.
			Background(theme.SecondaryBackground).
			BorderStyle(lipgloss.NormalBorder()).
			BorderForeground(theme.Accent).
			BorderRight(true)
	}
	leftPanelStyled := leftPaneStyle.Render(leftPanel)

	// Constrain right panel width so it doesn't crowd the left pane
	rightInnerWidth := m.contentWidth - 2 // account for border/padding
	if rightInnerWidth < 20 {
		rightInnerWidth = 20
	}
	rightWrapped := lipgloss.NewStyle().Width(rightInnerWidth).Render(rightPanelContent)
	var rightPanelStyled string
	if m.isRegisterTable() {
		rightPanelStyled = activeItemStyle.Copy().PaddingTop(2).Render(rightWrapped)
	} else {
		rightPanelStyled = activeItemStyle.Render(rightWrapped)
	}

	// Footer with column markers every 10th column
	totalWidth := m.leftPanelWidth + m.contentWidth
	footer := lipgloss.NewStyle().Width(totalWidth).Render(renderColumnMarkers(totalWidth))

	return lipgloss.JoinVertical(
		lipgloss.Left,
		header(),
		lipgloss.JoinHorizontal(
			lipgloss.Top,
			leftPanelStyled,
			rightPanelStyled,
		),
		footer,
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

// renderColumnMarkers returns a single-line string of length `width`
// with '|' at every 10th column (10,20,30,...), spaces elsewhere.
func renderColumnMarkers(width int) string {
	if width <= 0 {
		return ""
	}
	b := make([]rune, width)
	for i := 0; i < width; i++ {
		if (i+1)%10 == 0 {
			b[i] = '|'
		} else {
			b[i] = ' '
		}
	}
	return string(b)
}

// shouldHighlightLeftPane returns true when the selected left tab is one
// of the special cases which should visually distinguish the left pane
func (m *model) shouldHighlightLeftPane() bool {
	// Keep left pane appearance consistent across tabs
	return false
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
		WithSortAmount(m.settings.toggleSort).
		WithOutputCSV(true)

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

	// Calculate 8 months ago for accounts filter
	today := time.Now()
	eightMonthsAgo := today.AddDate(0, -8, 0)
	startDate := eightMonthsAgo.Format("2006-01-02")
	endDate := today.Format("2006-01-02")
	
	accountOpts := hledger.NewOptions().
		WithTree(m.settings.treeView).
		WithStartDate(startDate).
		WithEndDate(endDate)
	

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
					} else if m.months.IsActive() {
						// Month mode active – filter register to selected month in most-recent year available
						y, start, end := m.mostRecentYearRangeForMonth(m.months.SelectedMonthIndex())
						opts = hledger.NewOptions().
							WithStartDate(start).
							WithEndDate(end).
							WithOutputCSV(true)
						_ = y // unused in command layer, but kept for clarity
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
		case cmdBalanceSheet:
			// Build options to show last 3 months and current day, monthly, CSV, no commodity column post-processed
			// Start date: first day of month three months ago
			now := time.Now()
			threeMonthsAgo := now.AddDate(0, -3, 0)
			startOfThreeMonthsAgo := time.Date(threeMonthsAgo.Year(), threeMonthsAgo.Month(), 1, 0, 0, 0, 0, time.Local)
			// End date: tomorrow to include current day
			tomorrow := now.AddDate(0, 0, 1)
			opts = hledger.NewOptions().
				WithAccount(m.filterGroup.account.Value()).
				WithStartDate(startOfThreeMonthsAgo.Format("2006-01-02")).
				WithEndDate(tomorrow.Format("2006-01-02")).
				WithPeriod(hledger.PeriodMonthly).
				WithTree(m.settings.treeView).
				WithPretty(true).
				WithLayout(hledger.LayoutBare).
				WithAccountDrop(1).
				WithSortAmount(m.settings.toggleSort).
				WithOutputCSV(true)
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

// filterRegisterBySelectedMonth triggers a refresh constrained to the selected month
func (m *model) filterRegisterBySelectedMonth() tea.Cmd {
	return m.refresh()
}

func (m *model) mostRecentYearRangeForMonth(monIdx int) (int, string, string) {
	// Pick current year by default; in practice you could scan data to find most-recent year with that month
	now := time.Now()
	year := now.Year()
	mStart := time.Date(year, time.Month(monIdx+1), 1, 0, 0, 0, 0, time.Local)
	mEnd := mStart.AddDate(0, 1, -1)
	return year, mStart.Format("2006-01-02"), mEnd.Format("2006-01-02")
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

// handleDownNavigation: only cycle through tabs; no auto-graph navigation
func (m *model) handleDownNavigation() tea.Cmd {
	switch m.navigationMode {
	case NavTabs:
		m.tabs.incrementSelection()
		return nil
	case NavFilters:
		m.navigationMode = NavSettings
		return nil
	case NavSettings:
		// stay in settings; do nothing special
		return nil
	case NavGraphs:
		// when in graphs, move to next graph if available, otherwise wrap to first
		if m.graphView.selectedGraph < len(m.graphView.graphs)-1 {
			m.graphView.selectedGraph++
			m.graphView.currentGraph = m.graphView.graphs[m.graphView.selectedGraph].typ
			m.graphView.showGraph = true
		} else {
			m.graphView.selectedGraph = 0
			m.graphView.currentGraph = m.graphView.graphs[0].typ
			m.graphView.showGraph = true
		}
		return nil
	}
	return nil
}

// handleUpNavigation: only cycle through tabs; no auto-graph navigation
func (m *model) handleUpNavigation() tea.Cmd {
	switch m.navigationMode {
	case NavGraphs:
		// when in graphs, move to previous graph, wrap to last
		if m.graphView.selectedGraph > 0 {
			m.graphView.selectedGraph--
			m.graphView.currentGraph = m.graphView.graphs[m.graphView.selectedGraph].typ
			m.graphView.showGraph = true
		} else {
			m.graphView.selectedGraph = len(m.graphView.graphs) - 1
			m.graphView.currentGraph = m.graphView.graphs[m.graphView.selectedGraph].typ
			m.graphView.showGraph = true
		}
		return nil
	case NavSettings:
		m.navigationMode = NavFilters
		return nil
	case NavFilters:
		m.navigationMode = NavTabs
		return nil
	case NavTabs:
		m.tabs.decrementSelection()
		return nil
	}
	return nil
}
