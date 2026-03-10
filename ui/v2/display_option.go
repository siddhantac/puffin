package ui

import (
	"fmt"

	"github.com/siddhantac/puffin/ui/v2/interfaces"

	tea "github.com/charmbracelet/bubbletea"
	"github.com/charmbracelet/lipgloss"
)

type displayOption struct {
	name   string
	value  interface{}
	render func(style lipgloss.Style, name string, value interface{}) string
}

func valueRenderer(style lipgloss.Style, name string, value interface{}) string {
	return style.Render(fmt.Sprintf("%s: %v", name, value))
}

func boolRenderer(style lipgloss.Style, name string, value interface{}) string {
	if v, ok := value.(bool); ok && v {
		return style.Copy().Foreground(lipgloss.Color("#04B575")).Render(name)
	}
	return style.Render(name)
}

func interval(defaultInterval interfaces.Interval) *displayOption {
	return &displayOption{
		name:   "󰸘 interval",
		value:  defaultInterval,
		render: valueRenderer,
	}
}

func depth(defaultDepth int) *displayOption {
	return &displayOption{
		name:   " depth",
		value:  defaultDepth,
		render: valueRenderer,
	}
}

func sort(defaultSort bool) *displayOption {
	return &displayOption{
		name:   " sort",
		value:  defaultSort,
		render: boolRenderer,
	}
}

func average(enabled bool) *displayOption {
	return &displayOption{
		name:   "average",
		value:  enabled,
		render: boolRenderer,
	}
}

type displayOptionsGroup struct {
	interval *displayOption
	depth    *displayOption
	sort     *displayOption
	average  *displayOption
	options  []*displayOption
}

func (dg *displayOptionsGroup) SortValue() bool {
	if v, ok := dg.sort.value.(bool); ok {
		return v
	}
	return false
}

func (dg *displayOptionsGroup) DepthValue() int {
	if v, ok := dg.depth.value.(int); ok {
		return v
	}
	return 1
}

func (dg *displayOptionsGroup) IntervalValue() interfaces.Interval {
	if v, ok := dg.interval.value.(interfaces.Interval); ok {
		return v
	}
	return ""
}

func (dg *displayOptionsGroup) AverageValue() bool {
	if v, ok := dg.average.value.(bool); ok {
		return v
	}
	return false
}

func (dg *displayOptionsGroup) Init() tea.Cmd {
	return nil
}

func (dg *displayOptionsGroup) Update(msg tea.Msg) (tea.Model, tea.Cmd) {
	switch msg := msg.(type) {
	case tea.KeyMsg:
		switch msg.String() {
		case "m":
			if dg.interval != nil {
				dg.interval.value = interfaces.Monthly
				return dg, refreshDataCmd
			}
		case "y":
			if dg.interval != nil {
				dg.interval.value = interfaces.Yearly
				return dg, refreshDataCmd
			}
		case "+":
			if v, ok := dg.depth.value.(int); ok {
				dg.depth.value = v + 1
				return dg, refreshDataCmd
			}
		case "-":
			if v, ok := dg.depth.value.(int); ok {
				dg.depth.value = v - 1
				return dg, refreshDataCmd
			}

		case "s":
			if v, ok := dg.sort.value.(bool); ok {
				dg.sort.value = !v
				return dg, refreshDataCmd
			}

		case "a":
			if v, ok := dg.average.value.(bool); ok {
				dg.average.value = !v
				return dg, refreshDataCmd
			}
		default:
			return dg, nil
		}
	}
	return dg, nil
}

func (dg *displayOptionsGroup) View() string {
	style := lipgloss.NewStyle().
		PaddingLeft(1).
		PaddingRight(1).
		Border(lipgloss.RoundedBorder()).
		BorderForeground(lipgloss.Color("240"))

	var view string
	for _, f := range dg.options {
		view = lipgloss.JoinHorizontal(lipgloss.Left, view, f.render(style, f.name, f.value))
	}
	return view
}

type displayOptionsGroupFactory struct{}

func (f displayOptionsGroupFactory) NewHomeGroup(defaultDepth int, defaultSort bool) *displayOptionsGroup {
	dg := &displayOptionsGroup{
		depth: depth(defaultDepth),
		sort:  sort(defaultSort),
	}
	dg.options = []*displayOption{
		dg.depth,
		dg.sort,
	}
	return dg
}

func (f displayOptionsGroupFactory) NewReportsGroup(defaultInterval interfaces.Interval, defaultDepth int, defaultSort bool) *displayOptionsGroup {
	dg := &displayOptionsGroup{
		interval: interval(defaultInterval),
		depth:    depth(defaultDepth),
		sort:     sort(defaultSort),
		average:  average(false),
	}
	dg.options = []*displayOption{
		dg.interval,
		dg.depth,
		dg.sort,
		dg.average,
	}
	return dg
}

func (f displayOptionsGroupFactory) NewBalancesGroup(defaultInterval interfaces.Interval, defaultDepth int, defaultSort bool) *displayOptionsGroup {
	dg := &displayOptionsGroup{
		interval: interval(defaultInterval),
		depth:    depth(defaultDepth),
		sort:     sort(defaultSort),
		average:  average(false),
	}
	dg.options = []*displayOption{
		dg.interval,
		dg.depth,
		dg.sort,
		dg.average,
	}
	return dg
}
