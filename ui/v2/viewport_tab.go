package ui

import (
	"github.com/charmbracelet/bubbles/spinner"
	"github.com/charmbracelet/bubbles/viewport"
	tea "github.com/charmbracelet/bubbletea"
	"github.com/charmbracelet/lipgloss"
	"github.com/siddhantac/puffin/ui/v2/interfaces"
)

// rawIncomeProvider is a narrow interface satisfied by hledger.HledgerData.
type rawIncomeProvider interface {
	IncomeStatementRaw(filter interfaces.Filter, displayOptions interfaces.DisplayOptions) (string, error)
}

type queryViewportMsg struct{}
type updateViewportMsg struct{ content string }

func queryViewportCmd() tea.Msg { return queryViewportMsg{} }

type viewportTab struct {
	viewport            viewport.Model
	ready               bool
	loading             bool
	content             string
	spinner             spinner.Model
	filterGroup         *filterGroup
	displayOptionsGroup *displayOptionsGroup
	height, width       int
	dataProvider        rawIncomeProvider
	cmdRunner           *cmdRunner
}

func newViewportTab(dataProvider rawIncomeProvider, cmdRunner *cmdRunner) *viewportTab {
	optionFactory := displayOptionsGroupFactory{}
	filterGroupFactory := filterGroupFactory{}
	return &viewportTab{
		dataProvider:        dataProvider,
		cmdRunner:           cmdRunner,
		spinner:             newSpinner(),
		filterGroup:         filterGroupFactory.NewGroupReports(),
		displayOptionsGroup: optionFactory.NewReportsGroup(interfaces.Yearly, 3, interfaces.ByAccount),
	}
}

func (v *viewportTab) Init() tea.Cmd {
	return v.spinner.Tick
}

func (v *viewportTab) Update(msg tea.Msg) (tea.Model, tea.Cmd) {
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
			v.viewport = viewport.New(msg.Width, msg.Height-6)
			v.viewport.SetContent(v.content)
			v.ready = true
		} else {
			v.viewport.Width = msg.Width
			v.viewport.Height = msg.Height - 6
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
		return v, queryViewportCmd

	case queryViewportMsg:
		v.loading = true
		f := func() tea.Msg {
			content, err := v.fetchIncomeStatement()
			if err != nil {
				return updateViewportMsg{content: err.Error()}
			}
			return updateViewportMsg{content: content}
		}
		v.cmdRunner.Run(f)
		return v, nil

	case updateViewportMsg:
		v.loading = false
		v.content = msg.content
		if v.ready {
			v.viewport.SetContent(v.content)
		}
		return v, nil

	case tea.KeyMsg:
		if v.filterGroup.Focused() {
			fg, cmd := v.filterGroup.Update(msg)
			v.filterGroup = fg.(*filterGroup)
			return v, cmd
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

func (v *viewportTab) fetchIncomeStatement() (string, error) {
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
	return v.dataProvider.IncomeStatementRaw(filter, displayOptions)
}

func (v *viewportTab) View() string {
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

	var content string
	if v.loading {
		content = lipgloss.Place(
			v.width, v.height-6,
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
		content,
	)
}

