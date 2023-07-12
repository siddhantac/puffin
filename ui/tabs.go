package ui

import (
	"strings"

	tea "github.com/charmbracelet/bubbletea"
	"github.com/charmbracelet/lipgloss"
)

type Tabs struct {
	tabList []string
}

func newTabs() *Tabs {
	return &Tabs{
		tabList: []string{
			"register",
			"balance",
		},
	}
}

func (t *Tabs) Init() tea.Cmd {
	return nil
}

func (t *Tabs) View() string {
	renderedTabs := make([]string, 0)

	for _, t := range t.tabList {
		renderedTabs = append(renderedTabs, tabStyle.Render(t))
	}

	return lipgloss.NewStyle().
		Render(lipgloss.JoinHorizontal(lipgloss.Top, strings.Join(renderedTabs, tabSeparatorStyle.Render("|"))))
}
