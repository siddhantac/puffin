package ui

import (
	"fmt"
	"puffin/ui/v2/interfaces"

	tea "github.com/charmbracelet/bubbletea"
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

func (dg *displayOptionsGroup) IntervalValue() interfaces.Interval {
	mapping := map[string]interfaces.Interval{
		"monthly": interfaces.Monthly,
		"yearly":  interfaces.Yearly,
	}
	return mapping[dg.interval.value]
}

func (dg *displayOptionsGroup) Init() tea.Cmd {
	return nil
}

func (dg *displayOptionsGroup) Update(msg tea.Msg) (tea.Model, tea.Cmd) {
	switch msg := msg.(type) {
	case tea.KeyMsg:
		switch msg.String() {
		case "m":
			dg.interval.value = "monthly"
		case "y":
			dg.interval.value = "yearly"
		case "+":
			dg.depth.value++
		case "-":
			dg.depth.value--

		case "s":
			if dg.sort.value == "acct" {
				dg.sort.value = "amt"
			} else {
				dg.sort.value = "acct"
			}
		}
	}
	return dg, nil
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
