package ui

import (
	"strings"

	"github.com/charmbracelet/lipgloss"
)

type Tabs struct {
	tabList     []string
	selectedTab int
}

func newTabs() *Tabs {
	return &Tabs{
		selectedTab: 0,
		tabList: []string{
			"register",
			"balance",
		},
	}
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

	return lipgloss.NewStyle().
		Render(lipgloss.JoinHorizontal(lipgloss.Top, strings.Join(renderedTabs, tabSeparatorStyle.Render("|"))))
}
