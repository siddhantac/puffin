package ui

import (
	"fmt"
	"hledger/hledger"

	"github.com/charmbracelet/bubbles/textinput"
	tea "github.com/charmbracelet/bubbletea"
)

type filterType int

const (
	accountFilter filterType = iota
	dateFilter
)

type form struct {
	query      textinput.Model
	model      tea.Model
	filterType filterType
}

func newFilterForm(model tea.Model, filterType filterType) *form {
	f := &form{}
	f.model = model
	f.filterType = filterType
	f.query = textinput.New()
	f.query.Placeholder = "filter ('esc' to cancel)"
	f.query.Focus()
	return f
}

func (m *form) newFilter() tea.Msg {
	switch m.filterType {
	case accountFilter:
		return hledger.NewAccountFilter(m.query.Value())
	case dateFilter:
		return hledger.NewDateFilter().WithSmartText(m.query.Value())
	}
	return hledger.NewAccountFilter(m.query.Value())
}

func (m *form) Init() tea.Cmd {
	return nil
}

func (m *form) Update(msg tea.Msg) (tea.Model, tea.Cmd) {
	switch msg := msg.(type) {
	case tea.KeyMsg:
		switch msg.String() {
		case "esc", "q", "ctrl+c":
			return m.model, nil
		case "enter":
			return m.model, m.newFilter
		}
	}

	var cmd tea.Cmd
	m.query, cmd = m.query.Update(msg)

	return m, cmd
}

func (m *form) View() string {
	tbl := m.model.View()
	form := m.query.View()

	return fmt.Sprintf("%s%s", form, tbl)
}
