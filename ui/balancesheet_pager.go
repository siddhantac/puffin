package ui

import (
	"puffin/logger"

	"github.com/charmbracelet/bubbles/viewport"
	tea "github.com/charmbracelet/bubbletea"
)

type balanceSheetData string
type balanceSheetPager struct {
	viewport viewport.Model
	ready    bool
	content  string
}

func (p *balanceSheetPager) Init() tea.Cmd { return nil }
func (p *balanceSheetPager) Update(msg tea.Msg) (tea.Model, tea.Cmd) {
	var (
		cmd  tea.Cmd
		cmds []tea.Cmd
	)
	switch msg := msg.(type) {
	case tea.WindowSizeMsg:
		logger.Logf("pager received: %v", msg)
		if !p.ready {
			// Since this program is using the full size of the viewport we
			// need to wait until we've received the window dimensions before
			// we can initialize the viewport. The initial dimensions come in
			// quickly, though asynchronously, which is why we wait for them
			// here.
			p.viewport = viewport.New(msg.Width, msg.Height-9)
			p.viewport.YPosition = 10
			// i.m.HighPerformanceRendering = useHighPerformanceRenderer
			p.ready = true
			// i.viewport.SetContent("\nHello World")

		} else {
			p.viewport.Width = msg.Width
			p.viewport.Height = msg.Height - 9
		}
		logger.Logf("pager ready: %v", p.ready)

	case balanceSheetData:
		p.viewport.SetContent(string(msg))
	}

	// Handle keyboard and mouse events in the viewport
	p.viewport, cmd = p.viewport.Update(msg)
	cmds = append(cmds, cmd)

	return p, tea.Batch(cmds...)
}
func (p *balanceSheetPager) View() string {
	if !p.ready {
		return "\n  Initializing..."
	}
	return p.viewport.View()
}
