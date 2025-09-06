package ui

import (
	"strings"
	"github.com/siddhantac/puffin/ui/keys"

	"github.com/charmbracelet/bubbles/key"
	tea "github.com/charmbracelet/bubbletea"
)

type TabItem struct {
	name string
	item ContentModel
}

type Tabs struct {
	tabList     []TabItem
	selectedTab int
}

func newTabs(tabList []TabItem) *Tabs {
	return &Tabs{
		selectedTab: 0,
		tabList:     tabList,
	}
}

func (t *Tabs) Init() tea.Cmd { return nil }

func (t *Tabs) Update(msg tea.Msg) (tea.Model, tea.Cmd) {
	switch msg := msg.(type) {

	case tea.KeyMsg:
		switch {
		case key.Matches(msg, keys.Up):
			t.decrementSelection()
		case key.Matches(msg, keys.Down):
			t.incrementSelection()
		}
	}
	return t, nil
}

func (t *Tabs) View() string {
	// Render a vertical list with the active tab highlighted in theme blue
	lines := make([]string, 0, len(t.tabList))
	for i, tl := range t.tabList {
		label := "          " + tl.name // 10 spaces left pad to align with left pane
		if i == t.selectedTab {
			lines = append(lines, activeTabStyle.Render(label))
		} else {
			lines = append(lines, inactiveTabStyle.Render(label))
		}
	}
	return strings.Join(lines, "\n")
}

func (t *Tabs) CurrentTab() TabItem {
	return t.tabList[t.selectedTab]
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
