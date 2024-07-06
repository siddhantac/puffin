package ui

import tea "github.com/charmbracelet/bubbletea"

type genericContent struct {
	msg tea.Msg
	id  int
}

type genericPager struct {
	*pager
	id      int
	content tea.Msg
}

func newGenericPager(id int, name string) *genericPager {
	return &genericPager{
		pager: newPager(name),
		id:    id,
	}
}

func (p *genericPager) SetContent(genericContent genericContent) {
	if genericContent.id != p.id {
		return
	}
	p.content = genericContent.msg
}
