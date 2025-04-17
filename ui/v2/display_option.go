package ui

import (
	"fmt"

	"github.com/charmbracelet/lipgloss"
)

type displayOption struct {
	name  string
	value string
}

type displayOptionsGroup struct {
	interval displayOption
}

func newDisplayOptionsGroup(value string) *displayOptionsGroup {
	return &displayOptionsGroup{
		interval: displayOption{
			name:  "interval",
			value: value,
		},
	}
}

func (dg *displayOptionsGroup) View() string {
	return lipgloss.NewStyle().
		PaddingLeft(1).
		PaddingRight(1).
		Border(lipgloss.RoundedBorder()).
		BorderForeground(lipgloss.Color("240")).
		Render(fmt.Sprintf("%s: %s", dg.interval.name, dg.interval.value))
}
