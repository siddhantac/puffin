package ui

import (
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
		if !p.ready {
			p.viewport = viewport.New(msg.Width, msg.Height-9)
			p.viewport.YPosition = 10
			// i.m.HighPerformanceRendering = useHighPerformanceRenderer
			p.viewport.SetContent("\n  Initializing...")
			p.ready = true
		} else {
			p.viewport.Width = msg.Width
			p.viewport.Height = msg.Height - 9
		}

	case balanceSheetData:
		p.viewport.SetContent(string(msg))
	}

	// Handle keyboard and mouse events in the viewport
	p.viewport, cmd = p.viewport.Update(msg)
	cmds = append(cmds, cmd)

	return p, tea.Batch(cmds...)
}
func (p *balanceSheetPager) View() string { return p.viewport.View() }
