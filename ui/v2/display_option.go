package ui

import (
	"fmt"

	"github.com/charmbracelet/lipgloss"
)

type stringOrInt interface {
	string | int
}

type displayOption[T stringOrInt] struct {
	name  string
	value T
}

type displayOptionsGroup struct {
	interval displayOption[string]
	depth    displayOption[int]
	sort     displayOption[string]
}

func newDisplayOptionsGroup(defaultInterval string, defaultDepth int, defaultSort string) *displayOptionsGroup {
	return &displayOptionsGroup{
		interval: displayOption[string]{
			name:  "interval",
			value: defaultInterval,
		},
		depth: displayOption[int]{
			name:  "depth",
			value: defaultDepth,
		},
		sort: displayOption[string]{
			name:  "sort",
			value: defaultSort,
		},
	}
}

func (dg *displayOptionsGroup) View() string {
	intervalView := lipgloss.NewStyle().
		PaddingLeft(1).
		PaddingRight(1).
		Border(lipgloss.RoundedBorder()).
		BorderForeground(lipgloss.Color("240")).
		Render(fmt.Sprintf("%s: %s", dg.interval.name, dg.interval.value))

	depthView := lipgloss.NewStyle().
		PaddingLeft(1).
		PaddingRight(1).
		Border(lipgloss.RoundedBorder()).
		BorderForeground(lipgloss.Color("240")).
		Render(fmt.Sprintf("%s: %d", dg.depth.name, dg.depth.value))

	sortView := lipgloss.NewStyle().
		PaddingLeft(1).
		PaddingRight(1).
		Border(lipgloss.RoundedBorder()).
		BorderForeground(lipgloss.Color("240")).
		Render(fmt.Sprintf("%s: %s", dg.sort.name, dg.sort.value))

	return lipgloss.JoinHorizontal(
		lipgloss.Left,
		intervalView,
		depthView,
		sortView,
	)
}
