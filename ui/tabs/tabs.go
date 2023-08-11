package tabs

import (
	"puffin/ui/keys"
	"puffin/ui/styles"
	"strings"

	"github.com/charmbracelet/bubbles/key"
	tea "github.com/charmbracelet/bubbletea"
	"github.com/charmbracelet/lipgloss"
)

type Tabs struct {
	tabList     []string
	selectedTab int
	keys        keys.KeyMap
	width       int
}

func New(tabList []string) *Tabs {
	return &Tabs{
		selectedTab: 0,
		keys:        keys.AllKeys,
		tabList:     tabList,
	}
}

func (t *Tabs) Init() tea.Cmd { return nil }

func (t *Tabs) Update(msg tea.Msg) (tea.Model, tea.Cmd) {
	switch msg := msg.(type) {

	case tea.WindowSizeMsg:
		t.width = msg.Width
	case tea.KeyMsg:
		switch {
		case key.Matches(msg, t.keys.Left):
			t.decrementSelection()
		case key.Matches(msg, t.keys.Right):
			t.incrementSelection()
		}
	}
	return t, nil
}

func (t *Tabs) View() string {
	renderedTabs := make([]string, 0)

	for i, tl := range t.tabList {
		if i == t.selectedTab {
			renderedTabs = append(renderedTabs, styles.ActiveTabStyle.Render(tl))
		} else {
			renderedTabs = append(renderedTabs, styles.InactiveTabStyle.Render(tl))
		}
	}

	return lipgloss.NewStyle().
		Width(t.width).
		BorderTop(true).
		BorderForeground(styles.Theme.SecondaryForeground).
		BorderStyle(lipgloss.NormalBorder()).
		Render(lipgloss.JoinHorizontal(lipgloss.Top, strings.Join(renderedTabs, styles.TabSeparatorStyle.Render("|"))))
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
