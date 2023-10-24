package ui

import (
	"github.com/charmbracelet/bubbles/viewport"
	tea "github.com/charmbracelet/bubbletea"
)

type pager struct {
	viewport viewport.Model
	ready    bool
}

func (p *pager) SetContent(s string) {
	p.viewport.SetContent(s)
}

func (p *pager) Init() tea.Cmd { return nil }
func (p *pager) Update(msg tea.Msg) (tea.Model, tea.Cmd) {
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
	}

	// Handle keyboard and mouse events in the viewport
	p.viewport, cmd = p.viewport.Update(msg)
	cmds = append(cmds, cmd)

	return p, tea.Batch(cmds...)
}
func (p *pager) View() string {
	return p.viewport.View()
}
