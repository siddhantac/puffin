package ui

import (
	"github.com/charmbracelet/bubbles/viewport"
	tea "github.com/charmbracelet/bubbletea"
	"github.com/charmbracelet/lipgloss"
)

type pagerLoading struct{}

func setPagerLoading() tea.Msg {
	return pagerLoading{}
}

type pager struct {
	viewport viewport.Model
	ready    bool
	width    int
}

func (p *pager) SetContent(s string) {
	w := lipgloss.Width(s)
	if w > p.width {
		p.viewport.SetContent("\n !! window size too small to show all data")
		return
	}
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
		p.width = msg.Width
		headerHeight := lipgloss.Height(header())
		footerHeight := lipgloss.Height(newHelpModel().View())
		verticalMarginHeight := headerHeight + footerHeight
		if !p.ready {
			p.viewport = viewport.New(msg.Width, msg.Height-verticalMarginHeight)
			p.viewport.YPosition = headerHeight
			p.viewport.SetContent("\n  Loading...")
			// i.m.HighPerformanceRendering = useHighPerformanceRenderer
			p.ready = true
		} else {
			p.viewport.Width = msg.Width
			p.viewport.Height = msg.Height - verticalMarginHeight
		}
	case pagerLoading:
		p.viewport.SetContent("\n  Loading...")
	}

	// Handle keyboard and mouse events in the viewport
	p.viewport, cmd = p.viewport.Update(msg)
	cmds = append(cmds, cmd)

	return p, tea.Batch(cmds...)
}
func (p *pager) View() string {
	return p.viewport.View()
}
