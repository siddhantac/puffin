package ui

import (
	"fmt"
	"log"

	"github.com/siddhantac/puffin/ui/keys"

	"github.com/charmbracelet/bubbles/key"
	"github.com/charmbracelet/bubbles/viewport"
	tea "github.com/charmbracelet/bubbletea"
	"github.com/charmbracelet/lipgloss"
	"github.com/siddhantac/hledger"
)

type pager struct {
	id      int
	locked  bool
	cmd     func(int, hledger.Options) content
	cmdType cmdType

	viewport    viewport.Model
	width       int
	ready       bool
	isDataReady bool
	name        string
}

func newPager(id int, name string, locked bool, cmd func(int, hledger.Options) content, cmdType cmdType) *pager {
	return &pager{
		id:      id,
		locked:  locked,
		cmd:     cmd,
		cmdType: cmdType,
		name:    name,
	}
}

func (p *pager) log(msg string) {
	log.Printf("%s(%d): %s", p.name, p.id, msg)
}

func (p *pager) SetContent(gc content) {
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
	p.log("ready")
}

func (p *pager) IsReady() bool { return p.isDataReady }
func (p *pager) Type() cmdType { return p.cmdType }
func (p *pager) Locked() bool  { return p.locked }
func (p *pager) SetUnready() {
	p.isDataReady = false
	p.log("unready")
}

func (p *pager) Run(options hledger.Options) tea.Cmd {
	return func() tea.Msg {
		return p.cmd(p.id, options)
	}
}

func (p *pager) Init() tea.Cmd {
	p.SetUnready()
	return nil
}

func (p *pager) Update(msg tea.Msg) (tea.Model, tea.Cmd) {
	var (
		cmd  tea.Cmd
		cmds []tea.Cmd
	)
	switch msg := msg.(type) {
	case tea.WindowSizeMsg:
		p.width = msg.Width
		if !p.ready {
			p.log(fmt.Sprintf("height=%v, ypos=%v", p.viewport.Height, p.viewport.YPosition))
			p.viewport = viewport.New(msg.Width, msg.Height)
			p.ready = true
		} else {
			p.viewport.Width = msg.Width
			p.viewport.Height = msg.Height
		}

	case tea.KeyMsg:
		switch {
		case key.Matches(msg, keys.ScrollDown):
			p.viewport.LineDown(1)
		case key.Matches(msg, keys.ScrollUp):
			p.viewport.LineUp(1)
		}
	}

	// Handle keyboard and mouse events in the viewport
	p.viewport, cmd = p.viewport.Update(msg)
	cmds = append(cmds, cmd)

	return p, tea.Batch(cmds...)
}

func (p *pager) View() string { return p.viewport.View() }
