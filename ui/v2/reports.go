package ui

import (
	"github.com/charmbracelet/bubbles/spinner"
	"github.com/charmbracelet/bubbles/viewport"
	tea "github.com/charmbracelet/bubbletea"
	"github.com/charmbracelet/lipgloss"
	"github.com/siddhantac/puffin/ui/v2/interfaces"
)

// rawDataProvider is a narrow interface satisfied by hledger.HledgerData.
type rawDataProvider interface {
	IncomeStatementRaw(filter interfaces.Filter, displayOptions interfaces.DisplayOptions) (string, error)
	BalanceSheetRaw(filter interfaces.Filter, displayOptions interfaces.DisplayOptions) (string, error)
}

type queryReportsMsg struct{ index int }
type updateReportsMsg struct {
	index   int
	content string
}

func queryReportsCmd(index int) tea.Cmd {
	return func() tea.Msg { return queryReportsMsg{index: index} }
}

type reports struct {
	viewport            viewport.Model
	ready               bool
	spinner             spinner.Model
	filterGroup         *filterGroup
	displayOptionsGroup *displayOptionsGroup
	height, width       int
	dataProvider        rawDataProvider
	cmdRunner           *cmdRunner

	subTabTitles   []string
	subTabContents []string
	subTabLoading  []bool
	activeSubTab   int
}

func newReportsTab(dataProvider rawDataProvider, cmdRunner *cmdRunner) *reports {
	optionFactory := displayOptionsGroupFactory{}
	filterGroupFactory := filterGroupFactory{}
	return &reports{
		dataProvider:        dataProvider,
		cmdRunner:           cmdRunner,
		spinner:             newSpinner(),
		filterGroup:         filterGroupFactory.NewGroupReports(),
		displayOptionsGroup: optionFactory.NewReportsGroup(interfaces.Yearly, 3, false),
		subTabTitles:        []string{"income statement", "balance sheet"},
		subTabContents:      []string{"", ""},
		subTabLoading:       []bool{false, false},
		activeSubTab:        0,
	}
}

func (v *reports) Init() tea.Cmd {
	return v.spinner.Tick
}

func (v *reports) Update(msg tea.Msg) (tea.Model, tea.Cmd) {
	var cmd tea.Cmd
	switch msg := msg.(type) {
	case spinner.TickMsg:
		v.spinner, cmd = v.spinner.Update(msg)
		return v, cmd

	case tea.WindowSizeMsg:
		v.width = msg.Width
		v.height = msg.Height

		fg, _ := v.filterGroup.Update(msg)
		v.filterGroup = fg.(*filterGroup)

		if !v.ready {
			v.viewport = viewport.New(msg.Width, msg.Height-7)
			v.viewport.SetContent(v.subTabContents[v.activeSubTab])
			v.ready = true
		} else {
			v.viewport.Width = msg.Width
			v.viewport.Height = msg.Height - 7
		}
		return v, nil

	case focusFilterMsg:
		v.filterGroup.Focus()
		return v, nil

	case blurFilterMsg:
		v.filterGroup.Blur()
		return v, nil

	case refreshDataMsg:
		v.filterGroup.Blur()
		cmds := make([]tea.Cmd, len(v.subTabTitles))
		for i := range v.subTabTitles {
			cmds[i] = queryReportsCmd(i)
		}
		return v, tea.Batch(cmds...)

	case queryReportsMsg:
		v.subTabLoading[msg.index] = true
		idx := msg.index
		f := func() tea.Msg {
			content, err := v.fetchData(idx)
			if err != nil {
				return updateReportsMsg{index: idx, content: err.Error()}
			}
			return updateReportsMsg{index: idx, content: content}
		}
		v.cmdRunner.Run(f)
		return v, nil

	case updateReportsMsg:
		v.subTabLoading[msg.index] = false
		v.subTabContents[msg.index] = msg.content
		if v.ready && msg.index == v.activeSubTab {
			v.viewport.SetContent(v.subTabContents[v.activeSubTab])
		}
		return v, nil

	case tea.KeyMsg:
		if v.filterGroup.Focused() {
			fg, cmd := v.filterGroup.Update(msg)
			v.filterGroup = fg.(*filterGroup)
			return v, cmd
		}

		switch msg.String() {
		case "tab":
			n := len(v.subTabTitles)
			v.activeSubTab = (v.activeSubTab + 1) % n
			v.viewport.SetContent(v.subTabContents[v.activeSubTab])
			if v.subTabContents[v.activeSubTab] == "" && !v.subTabLoading[v.activeSubTab] {
				return v, queryReportsCmd(v.activeSubTab)
			}
			return v, nil
		case "shift+tab":
			n := len(v.subTabTitles)
			v.activeSubTab = (v.activeSubTab - 1 + n) % n
			v.viewport.SetContent(v.subTabContents[v.activeSubTab])
			if v.subTabContents[v.activeSubTab] == "" && !v.subTabLoading[v.activeSubTab] {
				return v, queryReportsCmd(v.activeSubTab)
			}
			return v, nil
		}

		dg, cmd := v.displayOptionsGroup.Update(msg)
		v.displayOptionsGroup = dg.(*displayOptionsGroup)
		if cmd != nil {
			return v, cmd
		}
	}

	v.viewport, cmd = v.viewport.Update(msg)
	return v, cmd
}

func (v *reports) fetchData(index int) (string, error) {
	filter := interfaces.Filter{
		Account:   v.filterGroup.AccountName(),
		DateStart: v.filterGroup.DateStart(),
		DateEnd:   v.filterGroup.DateEnd(),
	}
	displayOptions := interfaces.DisplayOptions{
		Interval: v.displayOptionsGroup.IntervalValue(),
		Depth:    v.displayOptionsGroup.DepthValue(),
		Sort:     v.displayOptionsGroup.SortValue(),
	}
	switch index {
	case 0:
		return v.dataProvider.IncomeStatementRaw(filter, displayOptions)
	case 1:
		return v.dataProvider.BalanceSheetRaw(filter, displayOptions)
	}
	return "", nil
}

func (v *reports) View() string {
	if !v.ready {
		return "\n  Loading..."
	}

	filterView := lipgloss.JoinHorizontal(
		lipgloss.Center,
		v.filterGroup.View(),
		" ",
		lipgloss.NewStyle().
			Border(lipgloss.RoundedBorder(), false, false, false, true).
			BorderForeground(lipgloss.Color("240")).
			Render(divider.View()),
		" ",
		v.displayOptionsGroup.View(),
	)

	titleStyle := lipgloss.NewStyle().PaddingLeft(1).PaddingRight(1)
	// activeTitleStyle := titleStyle.Copy().Background(lipgloss.Color("57"))

	renderedTitles := make([]string, len(v.subTabTitles))
	for i, t := range v.subTabTitles {
		if i == v.activeSubTab {
			renderedTitles[i] = activeTabStyle.Render(t)
		} else {
			renderedTitles[i] = titleStyle.Render(t)
		}
	}
	subTabBar := lipgloss.JoinHorizontal(lipgloss.Top, renderedTitles...)

	var content string
	if v.subTabLoading[v.activeSubTab] {
		content = lipgloss.Place(
			v.width, v.height-7,
			lipgloss.Center, lipgloss.Center,
			v.spinner.View(),
		)
	} else {
		content = lipgloss.NewStyle().
			PaddingLeft(2).
			Render(v.viewport.View())
	}

	return lipgloss.JoinVertical(
		lipgloss.Left,
		filterView,
		subTabBar,
		content,
	)
}
