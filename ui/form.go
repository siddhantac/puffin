package ui

import (
	"hledger/hledger"

	"github.com/charmbracelet/bubbles/textinput"
	tea "github.com/charmbracelet/bubbletea"
)

type form struct {
	query        textinput.Model
	focusedTable modelType
}

func newFilterForm(focusedTable modelType) *form {
	f := &form{}
	f.focusedTable = focusedTable
	f.query = textinput.New()
	f.query.Placeholder = "filter ('esc' to cancel)"
	f.query.Focus()
	return f
}

func (m *form) newAccountFilter() tea.Msg {
	return hledger.NewAccountFilter(m.query.Value())
}

func (m *form) Init() tea.Cmd {
	return nil
}

func (m *form) Update(msg tea.Msg) (tea.Model, tea.Cmd) {
	switch msg := msg.(type) {
	case tea.KeyMsg:
		switch msg.String() {
		case "esc":
			return models[m.focusedTable], nil
		case "q", "ctrl+c":
			return m, tea.Quit
		case "enter":
			return models[m.focusedTable], m.newAccountFilter
		}
	}

	var cmd tea.Cmd
	m.query, cmd = m.query.Update(msg)

	return m, cmd
}

func (m *form) View() string {
	return m.query.View()
}
