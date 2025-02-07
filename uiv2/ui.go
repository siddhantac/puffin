package uiv2

import (
	"fmt"
	"os"

	tea "github.com/charmbracelet/bubbletea"
	"github.com/charmbracelet/lipgloss"
)

func Start() {
	p := tea.NewProgram(newUI())
	if _, err := p.Run(); err != nil {
		fmt.Println("Error running program:", err)
		os.Exit(1)
	}
}

type ui struct {
	home tea.Model
	tabs tea.Model
}

func newUI() *ui {
	tabList := []tab{
		{name: "Home", model: Table{}},
		{name: "IS", model: Table{}},
		{name: "BS", model: Table{}},
	}
	return &ui{
		home: newHome(),
		tabs: NewTabList(tabList),
	}
}

func (u *ui) Init() tea.Cmd {
	return tea.EnterAltScreen
}

func (u *ui) Update(msg tea.Msg) (tea.Model, tea.Cmd) {
	var cmd tea.Cmd
	var batchCmds tea.BatchMsg

	u.tabs, cmd = u.tabs.Update(msg)
	batchCmds = append(batchCmds, cmd)

	u.home, cmd = u.home.Update(msg)
	batchCmds = append(batchCmds, cmd)

	switch msg := msg.(type) {
	case tea.KeyMsg:
		switch msg.String() {
		// case key.Matches(msg, keys.Quit):
		case "q":
			return u, tea.Quit
		}
	}

	return u, tea.Batch(
		batchCmds...,
	)

}

func (u *ui) View() string {
	content := lipgloss.JoinVertical(
		lipgloss.Center,
		u.tabs.View(),
		u.home.View(),
	)
	return content
}
