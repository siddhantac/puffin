package ui

import (
	tea "github.com/charmbracelet/bubbletea"
)

type genericContent struct {
	msg tea.Msg
	id  int
}

type genericPager struct {
	*pager
	id  int
	cmd tea.Cmd
}

func newGenericPager(id int, name string, cmd tea.Cmd) *genericPager {
	p := newPager(name)
	p.ready = true
	return &genericPager{
		pager: p,
		id:    id,
		cmd:   cmd,
	}
}

func (p *genericPager) Run() tea.Msg {
	return genericContent{
		id:  p.id,
		msg: p.cmd(),
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
	s, ok := gc.msg.(string)
	if ok {
		p.viewport.SetContent(s)
		p.isDataReady = true
	}
}

func (p *genericPager) IsReady() bool { return p.pager.IsReady() }
func (p *genericPager) SetUnready()   { return }

func (p *genericPager) Init() tea.Cmd { return p.pager.Init() }

func (p *genericPager) Update(msg tea.Msg) (tea.Model, tea.Cmd) { return p.pager.Update(msg) }

func (p *genericPager) View() string { return p.pager.View() }
