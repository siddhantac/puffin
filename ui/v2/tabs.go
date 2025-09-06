package ui

import (
	"log"

	tea "github.com/charmbracelet/bubbletea"
	"github.com/charmbracelet/lipgloss"
	"github.com/siddhantac/puffin/ui/colorscheme"
)

var tabStyle = lipgloss.NewStyle().
	PaddingLeft(1).
	PaddingRight(1)

var activeTabStyle = tabStyle.Copy().
	Bold(true).
	Background(lipgloss.Color(colorscheme.DraculaPurple)).
	Foreground(lipgloss.Color(colorscheme.DraculaForeground))

var inactiveTabStyle = tabStyle.Copy().
	Bold(false).
	Foreground(lipgloss.Color(colorscheme.GruvboxSkyBlue))

type tab struct {
	name  string
	model tea.Model
}

type tabList struct {
	selected int
	tabs     []*tab
}

func NewTabList(tabs []*tab) *tabList {
	return &tabList{
		selected: 0,
		tabs:     tabs,
	}
}

func (tl *tabList) Init() tea.Cmd {
	var batchCmds []tea.Cmd
	for _, t := range tl.tabs {
		batchCmds = append(batchCmds, t.model.Init())
	}
	return tea.Batch(batchCmds...)
	// return tl.tabs[tl.selected].model.Init()
}

func (tl *tabList) CurrentTab() *tab {
	return tl.tabs[tl.selected]
}
func (tl *tabList) NextTab() *tab {
	prev := tl.selected
	tl.selected++
	if tl.selected >= len(tl.tabs) {
		tl.selected = 0
	}
	log.Printf("tabs: tab: %s, previous tab: %s", tl.tabs[tl.selected].name, tl.tabs[prev].name)
	return tl.tabs[tl.selected]
}
func (tl *tabList) PrevTab() *tab {
	tl.selected--
	if tl.selected < 0 {
		tl.selected = len(tl.tabs) - 1
	}
	return tl.tabs[tl.selected]
}

func (tl *tabList) UpdateAll(msg tea.Msg) (*tabList, tea.Cmd) {
	var cmd tea.Cmd
	var batchCmds []tea.Cmd
	for i, t := range tl.tabs {
		t.model, cmd = t.model.Update(msg)
		tl.tabs[i] = t
		if cmd != nil {
			batchCmds = append(batchCmds, cmd)
		}
	}
	return tl, tea.Sequence(batchCmds...)
}

func (tl *tabList) Update(msg tea.Msg) (*tabList, tea.Cmd) {
	log.Printf("tabs: msg: %T | %v", msg, msg)
	var cmd tea.Cmd
	currentTab := tl.CurrentTab()
	currentTab.model, cmd = currentTab.model.Update(msg)
	tl.tabs[tl.selected] = currentTab
	log.Printf("tabs: current tab: %s", currentTab.name)
	return tl, cmd
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
