package ui

import (
	"log"
	"strconv"

	"github.com/charmbracelet/bubbles/viewport"
	tea "github.com/charmbracelet/bubbletea"
	"github.com/charmbracelet/lipgloss"
	"github.com/guptarohit/asciigraph"
	"github.com/siddhantac/hledger"
)

type size tea.WindowSizeMsg

type TableGraph struct {
	size         size
	tableSize    size
	viewportSize size

	name      string
	table     *Table
	viewport  viewport.Model
	id        int
	locked    bool
	cmd       func(options hledger.Options) string
	showGraph bool
	cmdType   cmdType
}

func newTableGraph(id int, name string, locked bool, cmd func(options hledger.Options) string, cmdType cmdType) *TableGraph {
	return &TableGraph{
		id:        id,
		name:      name,
		locked:    locked,
		cmd:       cmd,
		cmdType:   cmdType,
		table:     newTable(name, nil, id, cmd, locked, cmdType),
		viewport:  viewport.New(10, 10),
		showGraph: true,
	}
}

func (t *TableGraph) Run(options hledger.Options) tea.Cmd {
	return func() tea.Msg {
		return content{
			id:  t.id,
			msg: t.cmd(options),
		}
	}
}

func (t *TableGraph) Type() cmdType { return t.cmdType }
func (t *TableGraph) Locked() bool  { return t.locked }
func (t *TableGraph) IsReady() bool { return t.table.IsReady() }
func (t *TableGraph) SetUnready()   { t.table.SetUnready() }
func (t *TableGraph) SetContent(gc content) {
	if gc.id != t.id {
		return
	}
	log.Printf("tg: %s: setting content", t.name)

	t.table.SetContent(gc)
	t.setContentGraph()
}

func (t *TableGraph) Init() tea.Cmd {
	return nil
}

func (t *TableGraph) Update(msg tea.Msg) (tea.Model, tea.Cmd) {
	t.viewport.Update(msg)

	switch msg := msg.(type) {
	case tea.WindowSizeMsg:
		t.size = size{
			Width:  msg.Width,
			Height: msg.Height,
		}

		t.tableSize = size{
			Width:  t.size.Width,
			Height: percent(t.size.Height, 75),
		}
		t.viewportSize = size{
			Width:  t.size.Width,
			Height: percent(t.size.Height, 25),
		}

		t.table.Update(tea.WindowSizeMsg(t.tableSize))

		t.viewport = viewport.New(t.viewportSize.Width, t.viewportSize.Height)
		return t, nil

	case tea.KeyMsg:
		t.table.Update(msg)
		switch msg.String() {
		case "g":
			if t.showGraph {
				t.tableSize.Height = t.tableSize.Height + t.viewportSize.Height
			} else {
				t.tableSize.Height = percent(t.size.Height, 75)
			}
			t.showGraph = !t.showGraph
			t.table.Update(tea.WindowSizeMsg(t.tableSize))
		case "J", "K":
			row := t.table.SelectedRow()
			t.viewport.SetContent(t.plotGraph(strSliceToNumbers(row[2:]), row[0]))
		}
	}

	return t, nil
}

func (t *TableGraph) View() string {
	s := lipgloss.NewStyle().
		BorderStyle(lipgloss.NormalBorder()).
		BorderForeground(lipgloss.Color("240")).
		BorderBottom(true)

	if t.showGraph {
		return lipgloss.JoinVertical(
			lipgloss.Left,
			s.Render(t.table.View()),
			t.viewport.View(),
		)
	}
	return s.Render(t.table.View())
}

func (t *TableGraph) plotGraph(rows []float64, legend string) string {
	log.Printf("%s: %v", legend, rows)
	if rows == nil {
		return ""
	}
	graph := asciigraph.Plot(
		rows,
		asciigraph.SeriesColors(asciigraph.Red),
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
	row := t.table.SelectedRow()
	t.viewport.SetContent(t.plotGraph(strSliceToNumbers(row[2:]), row[0]))
}
