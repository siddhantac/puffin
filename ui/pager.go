package ui

import (
	"log"

	"github.com/charmbracelet/bubbles/viewport"
	tea "github.com/charmbracelet/bubbletea"
	"github.com/charmbracelet/lipgloss"
)

type pager struct {
	viewport    viewport.Model
	ready       bool
	width       int
	isDataReady bool
}

func newPager() *pager { return &pager{} }

func (p *pager) SetContent(msg tea.Msg) {
	s, ok := msg.(string)
	if !ok {
		return
	}

	w := lipgloss.Width(s)
	if w > p.width {
		p.viewport.SetContent("\n !! window size too small to show all data")
		return
	}
	p.viewport.SetContent(s)
	p.isDataReady = true
}

func (p *pager) IsReady() bool { return p.isDataReady }

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
		verticalMarginHeight := headerHeight + footerHeight
		log.Printf("pager: WindowSizeMsg: h=%v, headerHeight=%v, footerHeight=%v, verticalMarginHeight=%v", msg.Height, headerHeight, footerHeight, verticalMarginHeight)
		if !p.ready {
			p.viewport = viewport.New(msg.Width, msg.Height-verticalMarginHeight)
			p.viewport.YPosition = headerHeight
			p.ready = true
			log.Printf("pager: not-ready. height=%v, ypos=%v", p.viewport.Height, p.viewport.YPosition)
		} else {
			p.viewport.Width = msg.Width
			p.viewport.Height = msg.Height - verticalMarginHeight
			log.Printf("pager: ready. height=%v, ypos=%v", p.viewport.Height, p.viewport.YPosition)
		}
		// case pagerLoading:
		// 	p.viewport.SetContent("\n  Loading...")
	}

	// Handle keyboard and mouse events in the viewport
	p.viewport, cmd = p.viewport.Update(msg)
	cmds = append(cmds, cmd)

	return p, tea.Batch(cmds...)
}
func (p *pager) View() string {
	return p.viewport.View()
}

func (p *pager) SetUnready() { p.isDataReady = false }
