package ui

import (
	"fmt"
	"log"
	"strconv"

	"github.com/charmbracelet/bubbles/viewport"
	tea "github.com/charmbracelet/bubbletea"
	"github.com/charmbracelet/lipgloss"
	"github.com/guptarohit/asciigraph"
	"github.com/siddhantac/hledger"
)

type TableGraph struct {
	size         size
	tableSize    size
	viewportSize size

	name      string
	table     *Table
	viewport  viewport.Model
	id        int
	locked    bool
	cmd       func(int, hledger.Options) content
	showGraph bool
	cmdType   cmdType
}

func newTableGraph(id int, name string, locked bool, cmd func(int, hledger.Options) content, cmdType cmdType, dataTransformers []dataTransformer) *TableGraph {
	showGraph := true
	if locked {
		showGraph = false
	}
	return &TableGraph{
		id:        id,
		name:      name,
		locked:    locked,
		cmd:       cmd,
		cmdType:   cmdType,
		table:     newTable(name, nil, id, cmd, locked, cmdType, dataTransformers),
		viewport:  viewport.New(10, 10),
		showGraph: showGraph,
	}
}

func (t *TableGraph) Run(options hledger.Options) tea.Cmd {
	return func() tea.Msg {
		return t.cmd(t.id, options)
	}
}

func (t *TableGraph) log(msg string) {
	log.Printf("%s(%d): %s", t.name, t.id, msg)
}

func (t *TableGraph) Type() cmdType { return t.cmdType }
func (t *TableGraph) Locked() bool  { return t.locked }
func (t *TableGraph) IsReady() bool { return t.table.IsReady() }
func (t *TableGraph) SetUnready()   { t.table.SetUnready() }
func (t *TableGraph) SetContent(gc content) {
	if gc.id != t.id {
		return
	}
	t.log("setting content")

	t.table.SetContent(gc)
	t.setContentGraph()
}

func (t *TableGraph) Init() tea.Cmd {
	t.SetUnready()
	return nil
}

func (t *TableGraph) calculateTableHeight() int {
	if t.locked || !t.showGraph {
		return t.size.Height
	}

	// use subtraction instead of percent to avoid rounding issues
	// which can cause differences of 1 line
	return t.size.Height - t.viewportSize.Height
}

func (t *TableGraph) Update(msg tea.Msg) (tea.Model, tea.Cmd) {
	t.viewport.Update(msg)

	switch msg := msg.(type) {
	case tea.WindowSizeMsg:
		t.size = size{
			Width:  msg.Width,
			Height: msg.Height,
		}

		t.viewportSize = size{
			Width:  percent(t.size.Width, 99),
			Height: percent(t.size.Height, 25),
		}
		t.tableSize = size{
			Width:  t.size.Width,
			Height: t.calculateTableHeight(),
		}

		t.table.Update(tea.WindowSizeMsg(t.tableSize))
		t.viewport = viewport.New(t.viewportSize.Width, t.viewportSize.Height)

		t.log(fmt.Sprintf("windowSize: %s, tableSize: %s, viewportSize: %s", t.size, t.tableSize, t.viewportSize))

		return t, nil

	case tea.KeyMsg:
		t.table.Update(msg)
		switch msg.String() {
		case "g":
			if t.locked {
				return t, nil
			}

			t.showGraph = !t.showGraph
			t.tableSize.Height = t.calculateTableHeight()
			t.table.Update(tea.WindowSizeMsg(t.tableSize))

			t.log(fmt.Sprintf("showGraph: %v, tableSize: %v, size: %v, graph: %v", t.showGraph, t.tableSize, t.size, t.viewportSize))
		case "J", "K":
			row := t.table.SelectedRow()
			t.viewport.SetContent(t.plotGraph(strSliceToNumbers(row[2:]), row[0]))
		}
	}

	return t, nil
}

func (t *TableGraph) View() string {
	if !t.locked && t.showGraph {
		return lipgloss.JoinVertical(
			lipgloss.Left,
			t.table.View(),
			t.viewport.View(),
		)
	}
	return t.table.View()
}

func (t *TableGraph) plotGraph(rows []float64, legend string) string {
	if rows == nil {
		return ""
	}
	graph := asciigraph.Plot(
		rows,
		asciigraph.SeriesColors(asciigraph.IndianRed),
		asciigraph.Height(t.viewportSize.Height-3),
		asciigraph.Width(t.viewportSize.Width),
		asciigraph.SeriesLegends(legend),
	)
	return graph
}

func strSliceToNumbers(s []string) []float64 {
	var numbers []float64
	for _, v := range s {
		n, err := strconv.ParseFloat(v, 64)
		if err != nil {
			continue
		}
		numbers = append(numbers, n)
	}
	return numbers
}

func (t *TableGraph) setContentGraph() {
	if !t.table.IsReady() {
		return
	}

	if t.table.NumRows() < 1 {
		t.viewport.SetContent("")
		return
	}

	row := t.table.SelectedRow()
	if len(row) > 2 {
		t.viewport.SetContent(t.plotGraph(strSliceToNumbers(row[2:]), row[0]))
	}
}
