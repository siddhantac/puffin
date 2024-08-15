package ui

import (
	"log"

	"github.com/charmbracelet/bubbles/key"
	"github.com/charmbracelet/bubbles/spinner"
	"github.com/charmbracelet/bubbles/viewport"
	tea "github.com/charmbracelet/bubbletea"
	"github.com/charmbracelet/lipgloss"
	"github.com/siddhantac/hledger"
)

type pager struct {
	id      int
	locked  bool
	cmd     func(options hledger.Options) string
	cmdType cmdType

	viewport    viewport.Model
	ready       bool
	width       int
	isDataReady bool
	name        string
	spinner     spinner.Model
	keys        keyMap
}

func newPager(id int, name string, locked bool, cmd func(options hledger.Options) string, cmdType cmdType) *pager {
	return &pager{
		id:      id,
		locked:  locked,
		cmd:     cmd,
		cmdType: cmdType,
		name:    name,

		spinner: newSpinner(),
		keys:    allKeys,
	}
}

func (p *pager) SetContent(msg tea.Msg) {
	gc, ok := msg.(genericContent)
	if !ok {
		return
	}

	if gc.id != p.id {
		return
	}

	w := lipgloss.Width(gc.msg)
	if w > p.width {
		p.viewport.SetContent("\n ⚠️ window size too small to display all data\n\ttry to narrow down the results by date")
		p.isDataReady = true
		return
	}
	p.viewport.SetContent(gc.msg)
	p.isDataReady = true
	log.Printf("pager(%s): ready", p.name)
}

func (p *pager) IsReady() bool { return p.ready }
func (p *pager) Type() cmdType { return p.cmdType }
func (p *pager) Locked() bool  { return p.locked }
func (p *pager) SetUnready() {
	p.isDataReady = false
	log.Printf("pager(%s): set unready", p.name)
}

func (p *pager) Run(options hledger.Options) tea.Cmd {
	return func() tea.Msg {
		return genericContent{
			id:  p.id,
			msg: p.cmd(options),
		}
	}
}

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
		}
	case tea.WindowSizeMsg:
		p.width = msg.Width
		if !p.ready {
			log.Printf("pager(%s): not-ready. height=%v, ypos=%v", p.name, p.viewport.Height, p.viewport.YPosition)
			p.viewport = viewport.New(msg.Width, msg.Height)
			p.ready = true
		} else {
			p.viewport.Width = msg.Width
			p.viewport.Height = msg.Height
		}

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

func (p *pager) View() string { return p.viewport.View() }
