package ui

import (
	"fmt"
	"puffin/ui/v2/interfaces"

	tea "github.com/charmbracelet/bubbletea"
	"github.com/charmbracelet/lipgloss"
)

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

func sort(defaultSort string) *displayFilter {
	return &displayFilter{
		name:  "sort",
		value: defaultSort,
	}
}

type displayOptionsGroup struct {
	interval *displayFilter
	depth    *displayFilter
	sort     *displayFilter
	filters  []*displayFilter
}

func newDisplayOptionsGroup(defaultInterval string, defaultDepth int, defaultSort string) *displayOptionsGroup {
	dg := &displayOptionsGroup{
		interval: interval(defaultInterval),
		depth:    depth(defaultDepth),
		sort:     sort(defaultSort),
	}
	dg.filters = []*displayFilter{
		dg.interval,
		dg.depth,
		dg.sort,
	}
	return dg
}

func (dg *displayOptionsGroup) SortValue() string {
	if v, ok := dg.sort.value.(string); ok {
		return v
	}
	return ""
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
	// intervalView := lipgloss.NewStyle().
	// 	PaddingLeft(1).
	// 	PaddingRight(1).
	// 	Border(lipgloss.RoundedBorder()).
	// 	BorderForeground(lipgloss.Color("240")).
	// 	Render(fmt.Sprintf("%s: %v", dg.interval.name, dg.interval.value))
	//
	// depthView := lipgloss.NewStyle().
	// 	PaddingLeft(1).
	// 	PaddingRight(1).
	// 	Border(lipgloss.RoundedBorder()).
	// 	BorderForeground(lipgloss.Color("240")).
	// 	Render(fmt.Sprintf("%s: %d", dg.depth.name, dg.depth.value))
	//
	// sortView := lipgloss.NewStyle().
	// 	PaddingLeft(1).
	// 	PaddingRight(1).
	// 	Border(lipgloss.RoundedBorder()).
	// 	BorderForeground(lipgloss.Color("240")).
	// 	Render(fmt.Sprintf("%s: %s", dg.sort.name, dg.sort.value))

	style := lipgloss.NewStyle().
		PaddingLeft(1).
		PaddingRight(1).
		Border(lipgloss.RoundedBorder()).
		BorderForeground(lipgloss.Color("240"))

	var view string
	for _, f := range dg.filters {
		view = lipgloss.JoinHorizontal(lipgloss.Left,
			view,
			style.Render(fmt.Sprintf("%s: %v", f.name, f.value)),
		)
	}

	return view
	// return lipgloss.JoinHorizontal(
	// 	lipgloss.Left,
	// 	intervalView,
	// 	depthView,
	// 	sortView,
	// )
}
