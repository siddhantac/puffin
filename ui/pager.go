package ui

import (
	"log"

	"github.com/charmbracelet/bubbles/spinner"
	"github.com/charmbracelet/bubbles/viewport"
	tea "github.com/charmbracelet/bubbletea"
	"github.com/charmbracelet/lipgloss"
)

type pager struct {
	viewport    viewport.Model
	ready       bool
	width       int
	isDataReady bool
	name        string
	spinner     spinner.Model
}

func newPager(name string) *pager {
	return &pager{
		name:    name,
		spinner: newSpinner(),
	}
}

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

func (p *pager) IsReady() bool { return p.ready }

func (p *pager) Init() tea.Cmd { return p.spinner.Tick }

func (p *pager) Update(msg tea.Msg) (tea.Model, tea.Cmd) {
	var (
		cmd  tea.Cmd
		cmds []tea.Cmd
	)
	switch msg := msg.(type) {
	case spinner.TickMsg:
		if !p.isDataReady {
			p.spinner, cmd = p.spinner.Update(msg)
			cmds = append(cmds, cmd)
		}
		// log.Printf("pager(%s): spinner tick: %v", p.name, p.spinner.View())
	case tea.WindowSizeMsg:
		p.width = msg.Width
		headerHeight := lipgloss.Height(header())
		verticalMarginHeight := headerHeight + footerHeight
		log.Printf("pager(%s): WindowSizeMsg: h=%v, headerHeight=%v, footerHeight=%v, verticalMarginHeight=%v", p.name, msg.Height, headerHeight, footerHeight, verticalMarginHeight)
		if !p.ready {
			p.viewport = viewport.New(msg.Width, msg.Height-verticalMarginHeight)
			p.viewport.YPosition = headerHeight
			p.ready = true
			log.Printf("pager(%s): not-ready. height=%v, ypos=%v", p.name, p.viewport.Height, p.viewport.YPosition)
		} else {
			p.viewport.Width = msg.Width
			p.viewport.Height = msg.Height - verticalMarginHeight
			log.Printf("pager(%s): ready. height=%v, ypos=%v", p.name, p.viewport.Height, p.viewport.YPosition)
		}
		// case pagerLoading:
		// 	p.viewport.SetContent("\n  Loading...")
	}

	if !p.isDataReady {
		p.viewport.SetContent(p.spinner.View())
	}
	// Handle keyboard and mouse events in the viewport
	p.viewport, cmd = p.viewport.Update(msg)
	cmds = append(cmds, cmd)

	return p, tea.Batch(cmds...)
}
func (p *pager) View() string {
	// if !p.IsReady() {
	// 	return p.spinner.View()
	// }
	return p.viewport.View()
}

func (p *pager) SetUnready() { p.isDataReady = false }
