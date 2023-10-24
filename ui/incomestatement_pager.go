package ui

import (
	"github.com/charmbracelet/bubbles/viewport"
	tea "github.com/charmbracelet/bubbletea"
)

type incomeStatementPager struct {
	viewport viewport.Model
	ready    bool
}

func (p *incomeStatementPager) Init() tea.Cmd { return nil }
func (p *incomeStatementPager) Update(msg tea.Msg) (tea.Model, tea.Cmd) {
	var (
		cmd  tea.Cmd
		cmds []tea.Cmd
	)
	switch msg := msg.(type) {
	case tea.WindowSizeMsg:
		if !p.ready {
			p.viewport = viewport.New(msg.Width, msg.Height-9)
			p.viewport.YPosition = 10
			p.viewport.SetContent("\n  Initializing...")
			// i.m.HighPerformanceRendering = useHighPerformanceRenderer
			p.ready = true
		} else {
			p.viewport.Width = msg.Width
			p.viewport.Height = msg.Height - 9
		}

	case incomeStatementData:
		p.viewport.SetContent(string(msg))
	}

	// Handle keyboard and mouse events in the viewport
	p.viewport, cmd = p.viewport.Update(msg)
	cmds = append(cmds, cmd)

	return p, tea.Batch(cmds...)
}
func (p *incomeStatementPager) View() string {
	return p.viewport.View()
}
