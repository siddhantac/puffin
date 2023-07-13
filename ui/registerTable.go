package ui

import (
	"github.com/charmbracelet/bubbles/key"
	"github.com/charmbracelet/bubbles/table"
	tea "github.com/charmbracelet/bubbletea"
)

type registerTable struct {
	table.Model
	width int
}

func newRegisterTable(width int) *registerTable {
	rt := registerTable{}
	rt.SetWidth(width)
	rt.Model = newTable(rt.Columns())
	return &rt
}

func (r *registerTable) SetWidth(width int) {
	r.width = width - 20
}

func (r *registerTable) Columns() []table.Column {
	return []table.Column{
		{Title: "txnidx", Width: percent(r.width, 10)},
		{Title: "date", Width: percent(r.width, 15)},
		{Title: "description", Width: percent(r.width, 30)},
		{Title: "account", Width: percent(r.width, 30)},
		{Title: "amount", Width: percent(r.width, 15)},
	}
}

func (r *registerTable) Init() tea.Cmd {
	return nil
}

func (r *registerTable) Update(msg tea.Msg) (tea.Model, tea.Cmd) {
	switch msg := msg.(type) {
	case tea.WindowSizeMsg:
		r.SetWidth(msg.Width)
		r.Model = newTable(r.Columns())

	case tea.KeyMsg:
		switch {
		case key.Matches(msg, allKeys.Up):
			r.Model.MoveUp(1)
		case key.Matches(msg, allKeys.Down):
			r.Model.MoveDown(1)
		}
	}

	return r, nil
}

func (r *registerTable) View() string {
	return r.Model.View()
}
