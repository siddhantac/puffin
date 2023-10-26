package ui

import (
	"github.com/charmbracelet/bubbles/key"
	tea "github.com/charmbracelet/bubbletea"
	"github.com/charmbracelet/lipgloss"
)

type Tabs struct {
	tabList     []string
	selectedTab int
	help        helpModel
}

func newTabs() *Tabs {
	return &Tabs{
		selectedTab: 0,
		help:        newHelpModel(),
		tabList: []string{
			"assets",
			"expenses",
			"revenue",
			"liabilities",
			"income statement",
			"balance sheet",
			"register",
		},
	}
}

func (t *Tabs) Init() tea.Cmd { return nil }

func (t *Tabs) Update(msg tea.Msg) (tea.Model, tea.Cmd) {
	switch msg := msg.(type) {

	case tea.KeyMsg:
		switch {
		case key.Matches(msg, t.help.keys.Up), key.Matches(msg, t.help.keys.ShiftTab):
			t.decrementSelection()
		case key.Matches(msg, t.help.keys.Down), key.Matches(msg, t.help.keys.Tab):
			t.incrementSelection()
		}
	}
	return t, nil
}

func (t *Tabs) View() string {
	renderedTabs := make([]string, 0)

	for i, tl := range t.tabList {
		if i == t.selectedTab {
			renderedTabs = append(renderedTabs, activeTabStyle.Render(tl))
		} else {
			renderedTabs = append(renderedTabs, inactiveTabStyle.Render(tl))
		}
	}

	return tabGroupStyle.Render(lipgloss.JoinVertical(lipgloss.Right, renderedTabs...))
}

func (t *Tabs) CurrentTab() int {
	return t.selectedTab
}

func (t *Tabs) decrementSelection() {
	if t.selectedTab > 0 {
		t.selectedTab--
	} else {
		t.selectedTab = len(t.tabList) - 1
	}
}

func (t *Tabs) incrementSelection() {
	if t.selectedTab == len(t.tabList)-1 {
		t.selectedTab = 0
	} else {
		t.selectedTab++
	}
}
