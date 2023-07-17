package ui

import (
	"puffin/hledger"

	"github.com/charmbracelet/bubbles/textinput"
	tea "github.com/charmbracelet/bubbletea"
	"github.com/charmbracelet/lipgloss"
)

type filterType int

const (
	accountFilter filterType = iota
	dateFilter
	searchFilter
	periodFilter
)

type form struct {
	query      textinput.Model
	table      tea.Model
	filterType filterType
}

func newFilterForm(table tea.Model, filterType filterType) *form {
	f := &form{}
	f.table = table
	f.filterType = filterType
	f.query = textinput.New()
	f.query.Focus()

	switch filterType {
	case accountFilter:
		f.query.Placeholder = "account filter ('esc' to cancel)"
	case dateFilter:
		f.query.Placeholder = "date filter ('esc' to cancel)"
	case searchFilter:
		f.query.Placeholder = "desc filter ('esc' to cancel)"
	}
	return f
}

func (m *form) newFilter() tea.Msg {
	switch m.filterType {
	case accountFilter:
		return hledger.NewAccountFilter(m.query.Value())
	case dateFilter:
		return hledger.NewDateFilter().WithSmartDate(m.query.Value())
	case searchFilter:
		return hledger.NewDescriptionFilter(m.query.Value())
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
			return m.table, nil
		case "enter":
			return m.table, m.newFilter
		}
	}

	var cmd tea.Cmd
	m.query, cmd = m.query.Update(msg)

	return m, cmd
}

func (m *form) View() string {
	return lipgloss.JoinVertical(lipgloss.Left, m.query.View(), m.table.View())
}
