package ui

import (
	"fmt"
	"strings"
	"github.com/siddhantac/puffin/ui/keys"

	"github.com/charmbracelet/bubbles/key"
	tea "github.com/charmbracelet/bubbletea"
)

type settings struct {
	treeView     bool
	accountDepth int
	toggleSort   bool
	period       *Period
	theme        ThemeName
}

func newSettings(config Config) *settings {
	return &settings{
		treeView:     true,
		toggleSort:   false,
		accountDepth: 3,
		period:       newPeriod(config.PeriodType),
		theme:        GetCurrentTheme(),
	}
}

func (s *settings) Init() tea.Cmd {
	return nil
}

func (s *settings) Update(msg tea.Msg) (tea.Model, tea.Cmd) {
	switch msg := msg.(type) {
	case tea.KeyMsg:
		switch {
		case key.Matches(msg, keys.TreeView):
			s.treeView = !s.treeView
			return s, nil
		case key.Matches(msg, keys.AcctDepthDecr):
			s.accountDepth--
			if s.accountDepth < 1 {
				s.accountDepth = 1
			}
			return s, nil
		case key.Matches(msg, keys.AcctDepthIncr):
			s.accountDepth++
			return s, nil
		case key.Matches(msg, keys.SortBy):
			s.toggleSort = !s.toggleSort
			return s, nil
		case key.Matches(msg, keys.Theme):
			s.theme = NextTheme(s.theme)
			UpdateTheme(s.theme)
			return s, nil

		case key.Matches(
			msg,
			keys.Weekly,
			keys.Monthly,
			keys.Yearly,
			keys.Quarterly,
		):
			s.period.Update(msg)
			return s, nil
		}
	}
	return s, nil
}

func (s *settings) View() string {
	lines := []string{
		"     ",
		"      " + sectionTitleStyle.Render("SETTINGS"),
		"       " + s.theme.String(),
		" theme " + s.theme.String(),
		func() string { if s.treeView { return "  tree acct" } else { return "  tree off" } }(),
		"  sort " + func() string { if s.toggleSort { return "amt" } else { return "acct" } }(),
		" depth " + fmt.Sprintf("%d", s.accountDepth),
		"period " + s.period.String(),
		"────────────────",
	}
	return strings.Join(lines, "\n")
}
