package ui

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
	tabs *tabList
}

func newUI() *ui {
	home := newHome()

	tabList := []tab{
		{name: "Home", model: home},
		{name: "IS", model: Table{}},
		{name: "BS", model: Table{}},
		{name: "Details", model: newDetailView()},
	}
	return &ui{
		tabs: NewTabList(tabList),
	}
}

func (u *ui) Init() tea.Cmd {
	return tea.EnterAltScreen
}

func (u *ui) Update(msg tea.Msg) (tea.Model, tea.Cmd) {
	var cmd tea.Cmd
	var batchCmds tea.BatchMsg

	// u.tabs, cmd = u.tabs.Update(msg)
	// batchCmds = append(batchCmds, cmd)

	// u.home, cmd = u.home.Update(msg)
	// batchCmds = append(batchCmds, cmd)
	for _, t := range u.tabs.tabs {
		t.model, cmd = t.model.Update(msg)
		batchCmds = append(batchCmds, cmd)
	}

	switch msg := msg.(type) {
	case tea.KeyMsg:
		switch msg.String() {
		// case key.Matches(msg, keys.Quit):
		case "q":
			return u, tea.Quit
		case "]":
			u.tabs.NextTab()
			return u, nil
		case "[":
			u.tabs.PrevTab()
			return u, nil
		}
	}

	return u, tea.Batch(
		batchCmds...,
	)

}

func (u *ui) View() string {
	view := lipgloss.NewStyle().
		BorderStyle(lipgloss.NormalBorder()).
		BorderTop(true).
		BorderForeground(lipgloss.Color("240")).
		Render(u.tabs.CurrentTab().model.View())

	content := lipgloss.JoinVertical(
		lipgloss.Left,
		u.tabs.View(),
		view,
	)
	return content
}
