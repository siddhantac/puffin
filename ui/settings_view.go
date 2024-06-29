package ui

import (
	"fmt"

	"github.com/charmbracelet/bubbles/key"
	tea "github.com/charmbracelet/bubbletea"
	"github.com/charmbracelet/lipgloss"
)

type settings struct {
	keys         keyMap
	treeView     bool
	accountDepth int
	toggleSort   bool
}

func newSettings() *settings {
	return &settings{
		treeView:     true,
		toggleSort:   false,
		accountDepth: 3,
		keys:         allKeys,
	}
}

func (s *settings) Init() tea.Cmd {
	return nil
}

func (s *settings) Update(msg tea.Msg) (tea.Model, tea.Cmd) {
	switch msg := msg.(type) {
	case tea.KeyMsg:
		switch {
		case key.Matches(msg, s.keys.TreeView):
			s.treeView = !s.treeView
			return s, nil
		case key.Matches(msg, s.keys.AcctDepthDecr):
			s.accountDepth--
			if s.accountDepth < 1 {
				s.accountDepth = 1
			}
			return s, nil
		case key.Matches(msg, s.keys.AcctDepthIncr):
			s.accountDepth++
			return s, nil
		case key.Matches(msg, s.keys.SortBy):
			s.toggleSort = !s.toggleSort
			return s, nil
		}
	}
	return s, nil
}

func (s *settings) View() string {
	settingsTitleStyle := sectionTitleStyle.Copy().
		MarginTop(1).
		Render("SETTINGS")

	activeTextStyle := lipgloss.NewStyle().
		MarginRight(2)
	inactiveTextStyle := lipgloss.NewStyle().
		Foreground(theme.PrimaryForeground).
		MarginRight(2)

	accDepthTitle := inactiveTextStyle.Render("acct depth")
	accDepthValue := activeTextStyle.Render(fmt.Sprintf("%d", s.accountDepth))

	treeViewTitle := inactiveTextStyle.Render("tree")
	treeViewValue := activeTextStyle.Render(fmt.Sprintf("%t", s.treeView))

	sortModeTitle := inactiveTextStyle.Render("sort")
	var sortModeValue string
	if s.toggleSort {
		sortModeValue = activeTextStyle.Render("amt")
	} else {
		sortModeValue = activeTextStyle.Render("acct")
	}

	return lipgloss.JoinVertical(
		lipgloss.Right,
		settingsTitleStyle,
		treeViewTitle,
		treeViewValue,
		accDepthTitle,
		accDepthValue,
		sortModeTitle,
		sortModeValue,
	)
}
