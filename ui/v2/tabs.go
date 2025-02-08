package ui

import (
	tea "github.com/charmbracelet/bubbletea"
	"github.com/charmbracelet/lipgloss"
)

var tabStyle = lipgloss.NewStyle().
	PaddingLeft(1).
	PaddingRight(1)

var activeTabStyle = tabStyle.Copy().
	Bold(true).
	Background(lipgloss.Color("240"))
	// Foreground(theme.PrimaryColor)

var inactiveTabStyle = tabStyle.Copy().
	Bold(false)
	// Foreground(theme.SecondaryColor)

type tab struct {
	name  string
	model tea.Model
}

type tabList struct {
	selected int
	tabs     []tab
}

func NewTabList(tabs []tab) *tabList {
	return &tabList{
		selected: 0,
		tabs:     tabs,
	}
}
func (tl *tabList) CurrentTab() tab {
	return tl.tabs[tl.selected]
}
func (tl *tabList) NextTab() tab {
	tl.selected++
	if tl.selected >= len(tl.tabs) {
		tl.selected = 0
	}
	return tl.tabs[tl.selected]
}
func (tl *tabList) PrevTab() tab {
	tl.selected--
	if tl.selected < 0 {
		tl.selected = len(tl.tabs) - 1
	}
	return tl.tabs[tl.selected]
}

func (tl *tabList) View() string {
	renderedTabs := make([]string, 0)
	for i, t := range tl.tabs {
		if i == tl.selected {
			renderedTabs = append(renderedTabs, activeTabStyle.Render(t.name))
		} else {
			renderedTabs = append(renderedTabs, inactiveTabStyle.Render(t.name))
		}
	}
	return lipgloss.JoinHorizontal(lipgloss.Top, renderedTabs...)
}
