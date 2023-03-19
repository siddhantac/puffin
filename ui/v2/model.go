package v2

import (
	"github.com/charmbracelet/bubbles/key"
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
	sidebar Sidebar

	help Help

	screenHeight int
	screenWidth  int
	keys         KeyMap
	hlcmd        HledgerCmd
}

func NewModel(hlcmd HledgerCmd) Model {
	return Model{
		help:        NewHelp(),
		sidebar:     NewSidebar(hlcmd),
		mainSection: NewMainSection(hlcmd),
		keys:        Keys,
		hlcmd:       hlcmd,
	}
}

func (m Model) Init() tea.Cmd {
	return tea.Batch(
		m.help.Init(),
		m.sidebar.Init(),
		m.mainSection.Init(),
	)
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

	return m.updateChildComponents(msg)
}

func (m Model) updateChildComponents(msg tea.Msg) (tea.Model, tea.Cmd) {
	var (
		mainSectionCmd tea.Cmd
		helpCmd        tea.Cmd
		sidebarCmd     tea.Cmd
	)

	m.help, helpCmd = m.help.Update(msg)
	m.sidebar, sidebarCmd = m.sidebar.Update(msg)
	m.mainSection, mainSectionCmd = m.mainSection.Update(msg)

	return m, tea.Batch(
		mainSectionCmd,
		helpCmd,
		sidebarCmd,
	)
}

func (m Model) View() string {
	// dims := fmt.Sprintf("height=%d, width=%d\n", m.screenHeight, m.screenWidth)
	style := lipgloss.NewStyle().
		BorderStyle(lipgloss.NormalBorder()).
		BorderBottom(true).
		BorderTop(true).
		BorderLeft(true).
		BorderRight(true).
		BorderForeground(lipgloss.Color("66"))

	register := lipgloss.JoinVertical(
		lipgloss.Left,
		"Transactions",
		m.mainSection.View(),
	)

	balance := lipgloss.JoinVertical(
		lipgloss.Left,
		"Balance",
		m.sidebar.View(),
	)

	return style.Render(lipgloss.JoinVertical(
		lipgloss.Left,
		lipgloss.JoinHorizontal(
			lipgloss.Center,
			register,
			balance,
		),
		m.help.View(),
	))
}
