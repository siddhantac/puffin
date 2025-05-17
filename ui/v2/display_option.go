package ui

import (
	"fmt"

	"github.com/siddhantac/puffin/ui/v2/interfaces"

	tea "github.com/charmbracelet/bubbletea"
	"github.com/charmbracelet/lipgloss"
)

type displayOption struct {
	name  string
	value interface{}
}

func interval(defaultInterval interfaces.Interval) *displayOption {
	return &displayOption{
		name:  "interval",
		value: defaultInterval,
	}
}

func depth(defaultDepth int) *displayOption {
	return &displayOption{
		name:  "depth",
		value: defaultDepth,
	}
}

func sort(defaultSort string) *displayOption {
	return &displayOption{
		name:  "sort",
		value: defaultSort,
	}
}

type displayOptionsGroup struct {
	interval *displayOption
	depth    *displayOption
	sort     *displayOption
	options  []*displayOption
}

func (dg *displayOptionsGroup) SortValue() interfaces.SortBy {
	if v, ok := dg.sort.value.(interfaces.SortBy); ok {
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
	if v, ok := dg.interval.value.(interfaces.Interval); ok {
		return v
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
			if dg.sort.value == interfaces.ByAccount {
				dg.sort.value = interfaces.ByAmount
			} else {
				dg.sort.value = interfaces.ByAccount
			}
			return dg, refreshDataCmd

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
		view = lipgloss.JoinHorizontal(lipgloss.Left,
			view,
			style.Render(fmt.Sprintf("%s: %v", f.name, f.value)),
		)
	}

	return view
}

type displayOptionsGroupFactory struct{}

func (f displayOptionsGroupFactory) NewHomeGroup(defaultDepth int, defaultSort interfaces.SortBy) *displayOptionsGroup {
	dg := &displayOptionsGroup{
		depth: depth(defaultDepth),
		sort:  sort(string(defaultSort)),
	}
	dg.options = []*displayOption{
		dg.depth,
		dg.sort,
	}
	return dg
}

func (f displayOptionsGroupFactory) NewReportsGroup(defaultInterval interfaces.Interval, defaultDepth int, defaultSort interfaces.SortBy) *displayOptionsGroup {
	dg := &displayOptionsGroup{
		interval: interval(defaultInterval),
		depth:    depth(defaultDepth),
		sort:     sort(string(defaultSort)),
	}
	dg.options = []*displayOption{
		dg.interval,
		dg.depth,
		dg.sort,
	}
	return dg
}

func (f displayOptionsGroupFactory) NewBalancesGroup(defaultInterval interfaces.Interval, defaultDepth int, defaultSort interfaces.SortBy) *displayOptionsGroup {
	dg := &displayOptionsGroup{
		interval: interval(defaultInterval),
		depth:    depth(defaultDepth),
		sort:     sort(string(defaultSort)),
	}
	dg.options = []*displayOption{
		dg.interval,
		dg.depth,
		dg.sort,
	}
	return dg
}
