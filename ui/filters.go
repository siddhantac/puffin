package ui

import (
	"github.com/charmbracelet/bubbles/textinput"
	tea "github.com/charmbracelet/bubbletea"
)

type filterPanel struct {
	query textinput.Model
}

func newFilterPanel() *filterPanel {
	return &filterPanel{
		query: textinput.New(),
	}
}

func (f *filterPanel) Init() tea.Cmd {
	return nil
}

func (f *filterPanel) Update(msg tea.Msg) (tea.Model, tea.Cmd) {
	return nil, nil
}

func (f *filterPanel) View() string {
	return f.query.View()
}
