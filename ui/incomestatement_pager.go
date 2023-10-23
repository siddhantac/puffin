package ui

import (
	"puffin/logger"

	"github.com/charmbracelet/bubbles/viewport"
	tea "github.com/charmbracelet/bubbletea"
)

type incomeStatementPager struct {
	viewport viewport.Model
	ready    bool
	content  string
}

func (i *incomeStatementPager) Init() tea.Cmd { return nil }
func (i *incomeStatementPager) Update(msg tea.Msg) (tea.Model, tea.Cmd) {
	var (
		cmd  tea.Cmd
		cmds []tea.Cmd
	)
	switch msg := msg.(type) {
	case tea.WindowSizeMsg:
		logger.Logf("pager received: %v", msg)
		if !i.ready {
			// Since this program is using the full size of the viewport we
			// need to wait until we've received the window dimensions before
			// we can initialize the viewport. The initial dimensions come in
			// quickly, though asynchronously, which is why we wait for them
			// here.
			i.viewport = viewport.New(msg.Width, msg.Height-9)
			i.viewport.YPosition = 10
			// i.m.HighPerformanceRendering = useHighPerformanceRenderer
			i.ready = true
			i.viewport.SetContent("\nHello World")

		} else {
			i.viewport.Width = msg.Width
			i.viewport.Height = msg.Height - 9
		}
		logger.Logf("pager ready: %v", i.ready)

	case incomeStatementData2:
		i.content = string(msg)
		logger.Logf("incomes statment 2 recvd")
	}

	// Handle keyboard and mouse events in the viewport
	i.viewport, cmd = i.viewport.Update(msg)
	cmds = append(cmds, cmd)

	return i, tea.Batch(cmds...)
}
func (i *incomeStatementPager) View() string {
	if !i.ready {
		return "\n  Initializing..."
	}
	return i.content
	// return i.viewport.View()
}
