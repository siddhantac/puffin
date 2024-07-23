package ui

import (
	tea "github.com/charmbracelet/bubbletea"
	"github.com/siddhantac/hledger"
)

type genericContent struct {
	msg string
	id  int
}

type genericPager struct {
	*pager
	id     int
	locked bool
	cmd    func(options hledger.Options) string
}

func newGenericPager(id int, name string, locked bool, cmd func(options hledger.Options) string) *genericPager {
	p := newPager(name)
	p.ready = true
	return &genericPager{
		pager:  p,
		id:     id,
		locked: locked,
		cmd:    cmd,
	}
}

func (p *genericPager) Run(options hledger.Options) tea.Cmd {
	return func() tea.Msg {
		return genericContent{
			id:  p.id,
			msg: p.cmd(options),
		}
	}
}

func (p *genericPager) SetContent(msg tea.Msg) {
	gc, ok := msg.(genericContent)
	if !ok {
		return
	}

	if gc.id != p.id {
		return
	}

	p.viewport.SetContent(gc.msg)
	p.isDataReady = true
}

func (p *genericPager) IsReady() bool { return p.pager.IsReady() }
func (p *genericPager) SetUnready()   { p.pager.SetUnready() }

func (p *genericPager) Init() tea.Cmd { return p.pager.Init() }

func (p *genericPager) Update(msg tea.Msg) (tea.Model, tea.Cmd) { return p.pager.Update(msg) }

func (p *genericPager) View() string { return p.pager.View() }
