package ui

import (
	"github.com/charmbracelet/bubbles/help"
	tea "github.com/charmbracelet/bubbletea"
	"github.com/charmbracelet/lipgloss"
)

type helpModel struct {
	help       help.Model
	keys       keyMap
	inputStyle lipgloss.Style
	lastKey    string
	quitting   bool
}

func newHelpModel() helpModel {
	return helpModel{
		keys:       allKeys,
		help:       help.New(),
		inputStyle: lipgloss.NewStyle().Foreground(lipgloss.Color("#FF75B7")),
	}
}

func (m helpModel) Init() tea.Cmd {
	return nil
}

func (m helpModel) Update(msg tea.Msg) (tea.Model, tea.Cmd) {
	switch msg := msg.(type) {
	case tea.WindowSizeMsg:
		// If we set a width on the help menu it can it can gracefully truncate
		// its view as needed.
		m.help.Width = msg.Width
	}

	return m, nil
}

func (m helpModel) View() string {
	if m.quitting {
		return "Bye!\n"
	}

	style := lipgloss.NewStyle().MarginTop(1)
	return style.Render(m.help.View(m.keys))
}
