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
	period       *Period
}

func newSettings(config Config) *settings {
	return &settings{
		treeView:     true,
		toggleSort:   false,
		accountDepth: 3,
		period:       newPeriod(config.PeriodType),
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

		case key.Matches(
			msg,
			s.keys.Weekly,
			s.keys.Monthly,
			s.keys.Yearly,
			s.keys.Quarterly,
		):
			s.period.Update(msg)
			return s, nil
		}
	}
	return s, nil
}

func (s *settings) View() string {
	valueLength := 4

	settingsTitleStyle := sectionTitleStyle.Copy().
		Render("SETTINGS")

	activeTextStyle := lipgloss.NewStyle().
		MarginRight(0)
	inactiveTextStyle := lipgloss.NewStyle().
		Foreground(theme.PrimaryForeground).
		MarginRight(0)

	accDepthTitle := inactiveTextStyle.Render("depth ")
	accDepthValue := activeTextStyle.Render(fmt.Sprintf("%d", s.accountDepth))

	treeViewTitle := inactiveTextStyle.Render("tree ")
	var treeViewValue string
	if s.treeView {
		treeViewValue = activeTextStyle.Render(fmt.Sprintf("%-*s", valueLength, "on"))
	} else {
		treeViewValue = activeTextStyle.Render(fmt.Sprintf("%-*s", valueLength, "off"))
	}

	sortModeTitle := inactiveTextStyle.Render("sort ")
	var sortModeValue string
	if s.toggleSort {
		sortModeValue = activeTextStyle.Render(fmt.Sprintf("%-*s", valueLength, "amt"))
	} else {
		sortModeValue = activeTextStyle.Render(fmt.Sprintf("%-*s", valueLength, "acct"))
	}

	periodViewTitle := inactiveTextStyle.Render("period ")
	periodViewValue := activeTextStyle.Render(fmt.Sprintf("%-*s", valueLength, s.period.String()))

	settingsBlock := lipgloss.NewStyle().
		MarginRight(2).Render(
		lipgloss.JoinHorizontal(
			lipgloss.Center,
			lipgloss.JoinVertical(
				lipgloss.Right,
				treeViewTitle,
				sortModeTitle,
				accDepthTitle,
				periodViewTitle,
			),
			lipgloss.JoinVertical(
				lipgloss.Left,
				treeViewValue,
				sortModeValue,
				accDepthValue,
				periodViewValue,
			),
		),
	)

	return lipgloss.JoinVertical(
		lipgloss.Right,
		settingsTitleStyle,
		settingsBlock,
	)
}
