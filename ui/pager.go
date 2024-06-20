package ui

import (
	"log"

	"github.com/charmbracelet/bubbles/key"
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
	keys        keyMap
}

func newPager(name string) *pager {
	return &pager{
		name:    name,
		spinner: newSpinner(),
		keys:    allKeys,
	}
}

func (p *pager) SetContent(msg tea.Msg) {
	s, ok := msg.(string)
	if !ok {
		return
	}

	w := lipgloss.Width(s)
	if w > p.width {
		p.viewport.SetContent("\n ⚠️ window size too small to display all data\n\ttry to narrow down the results by date")
		p.isDataReady = true
		return
	}
	p.viewport.SetContent(s)
	p.isDataReady = true
	log.Printf("pager(%s): ready", p.name)
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
		p.spinner, cmd = p.spinner.Update(msg)
		cmds = append(cmds, cmd)
		if !p.isDataReady {
			p.viewport.SetContent(p.spinner.View())
			log.Printf("pager(%s): showing spinner", p.name)
		}
	case tea.WindowSizeMsg:
		p.width = msg.Width
		headerHeight := lipgloss.Height(header())
		verticalMarginHeight := headerHeight + footerHeight
		log.Printf("pager(%s): WindowSizeMsg: h=%v, headerHeight=%v, footerHeight=%v, verticalMarginHeight=%v", p.name, msg.Height, headerHeight, footerHeight, verticalMarginHeight)
		if !p.ready {
			log.Printf("pager(%s): not-ready. height=%v, ypos=%v", p.name, p.viewport.Height, p.viewport.YPosition)
			p.viewport = viewport.New(msg.Width, msg.Height-verticalMarginHeight)
			p.viewport.YPosition = headerHeight
			p.ready = true
		} else {
			p.viewport.Width = msg.Width
			p.viewport.Height = msg.Height - verticalMarginHeight
		}
		// case pagerLoading:
		// 	p.viewport.SetContent("\n  Loading...")

	case tea.KeyMsg:
		switch {
		case key.Matches(msg, p.keys.ScrollDown):
			log.Printf("pager(%s): scrolling down", p.name)
			p.viewport.LineDown(1)
		case key.Matches(msg, p.keys.ScrollUp):
			p.viewport.LineUp(1)
			log.Printf("pager(%s): scrolling up", p.name)
		}
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

func (p *pager) SetUnready() {
	p.isDataReady = false
	log.Printf("pager(%s): set unready", p.name)
}
