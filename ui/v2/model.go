package v2

import (
	"fmt"

	"github.com/charmbracelet/bubbles/key"
	"github.com/charmbracelet/bubbles/viewport"
	tea "github.com/charmbracelet/bubbletea"
	"github.com/charmbracelet/lipgloss"
)

/*
 Useful color codes
 8, 17, 18, 19, 60, 66, 96, 102, 103, 104, 108
*/

type Model struct {
	// tabs - expense detailed view, income detailed view etc, import, export

	// register table
	mainSection MainSection

	// sidebar - balances
	sidebar viewport.Model

	help Help

	screenHeight int
	screenWidth  int
	keys         KeyMap
}

func NewModel() Model {
	return Model{
		mainSection: NewMainSection(),
		help:        NewHelp(),
		keys:        Keys,
		// sidebar:     viewport.New(),
	}
}

func (m Model) Init() tea.Cmd {
	return nil
}

func (m Model) Update(msg tea.Msg) (tea.Model, tea.Cmd) {
	switch msg := msg.(type) {
	case tea.WindowSizeMsg:
		m.screenHeight = msg.Height
		m.screenWidth = msg.Width

	case tea.KeyMsg:
		switch {
		case key.Matches(msg, m.keys.Quit):
			return m, tea.Quit
		}
	}

	var (
		mainSectionCmd tea.Cmd
		helpCmd        tea.Cmd
	)

	m.mainSection, mainSectionCmd = m.mainSection.Update(msg)
	m.help, helpCmd = m.help.Update(msg)

	return m, tea.Batch(mainSectionCmd, helpCmd)
}

func (m Model) View() string {
	dims := fmt.Sprintf("height=%d, width=%d\n", m.screenHeight, m.screenWidth)
	return lipgloss.JoinVertical(
		lipgloss.Left,
		dims,
		m.mainSection.View(),
		m.help.View(),
	)
}
