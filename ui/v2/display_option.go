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

type displayFilter struct {
	name  string
	value interface{}
}

func interval(defaultInterval string) *displayFilter {
	return &displayFilter{
		name:  "interval",
		value: defaultInterval,
	}
}

func depth(defaultDepth int) *displayFilter {
	return &displayFilter{
		name:  "depth",
		value: defaultDepth,
	}
}

type displayOptionsGroup struct {
	sort displayOption[string]

	interval *displayFilter
	depth    *displayFilter
	filters  []*displayFilter
}

func newDisplayOptionsGroup(defaultInterval string, defaultDepth int, defaultSort string) *displayOptionsGroup {
	dg := &displayOptionsGroup{
		sort: displayOption[string]{
			name:  "sort",
			value: defaultSort,
		},

		interval: interval(defaultInterval),
		depth:    depth(defaultDepth),
	}
	dg.filters = []*displayFilter{dg.interval}
	return dg
}

func (dg *displayOptionsGroup) SortValue() string {
	return dg.sort.value
}

func (dg *displayOptionsGroup) DepthValue() int {
	if v, ok := dg.depth.value.(int); ok {
		return v
	}
	return 1
}

func (dg *displayOptionsGroup) IntervalValue() interfaces.Interval {
	mapping := map[string]interfaces.Interval{
		"monthly": interfaces.Monthly,
		"yearly":  interfaces.Yearly,
	}
	if v, ok := dg.interval.value.(string); ok {
		return mapping[v]
	}
	return ""
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
			if v, ok := dg.depth.value.(int); ok {
				dg.depth.value = v + 1
			}
		case "-":
			if v, ok := dg.depth.value.(int); ok {
				dg.depth.value = v - 1
			}

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
		Render(fmt.Sprintf("%s: %v", dg.interval.name, dg.interval.value))

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
