package ui

import (
	"encoding/csv"
	"errors"
	"io"
	"log"
	"strconv"
	"strings"

	"github.com/charmbracelet/bubbles/table"
	"github.com/charmbracelet/bubbles/viewport"
	tea "github.com/charmbracelet/bubbletea"
	"github.com/charmbracelet/lipgloss"
	"github.com/guptarohit/asciigraph"
	"github.com/siddhantac/hledger"
)

type size struct {
	width  int
	height int
}

type TableGraph struct {
	size         size
	tableSize    size
	viewportSize size

	table    *Table
	viewport viewport.Model
	id       int
	locked   bool
	cmd      func(options hledger.Options) string
}

func newTableGraph(id int, name string, locked bool, cmd func(options hledger.Options) string) *TableGraph {
	return &TableGraph{
		id:       id,
		locked:   locked,
		cmd:      cmd,
		table:    newTable(nil),
		viewport: viewport.New(10, 10),
	}
}

func (t *TableGraph) Run(options hledger.Options) tea.Cmd {
	return func() tea.Msg {
		return genericContent{
			id:  t.id,
			msg: t.cmd(options),
		}
	}
}

func (t *TableGraph) IsReady() bool { return t.table.IsReady() }
func (t *TableGraph) SetUnready()   { t.table.SetUnready() }
func (t *TableGraph) SetContent(msg tea.Msg) {
	gc, ok := msg.(genericContent)
	if !ok {
		return
	}

	if gc.id != t.id {
		return
	}

	t.setContentTable(gc.msg)
	t.setContentGraph()
}

func (t *TableGraph) Init() tea.Cmd {
	return nil
}

func (t *TableGraph) Update(msg tea.Msg) (tea.Model, tea.Cmd) {

	t.table.Update(msg)
	t.viewport.Update(msg)

	switch msg := msg.(type) {
	case tea.WindowSizeMsg:
		t.size = size{
			width:  msg.Width,
			height: msg.Height,
		}

		t.tableSize = size{
			width:  t.size.width,
			height: percent(t.size.height, 75),
		}
		t.viewportSize = size{
			width:  t.size.width,
			height: percent(t.size.height, 25),
		}

		windowSizeMsg := tea.WindowSizeMsg{
			Width:  t.tableSize.width,
			Height: t.tableSize.height,
		}
		t.table.Update(windowSizeMsg)

		t.viewport = viewport.New(t.viewportSize.width, t.viewportSize.height)
		return t, nil

	case tea.KeyMsg:
		switch msg.String() {
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

	return lipgloss.JoinVertical(lipgloss.Left, s.Render(t.table.View()), t.viewport.View())
}

func (t *TableGraph) plotGraph(rows []float64, legend string) string {
	log.Printf("%s: %v", legend, rows)
	graph := asciigraph.Plot(
		rows,
		asciigraph.SeriesColors(asciigraph.Red),
		asciigraph.Height(t.viewportSize.height-3),
		asciigraph.Width(t.viewportSize.width),
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

func parseCSV(r io.Reader) ([]table.Row, error) {
	result := make([]table.Row, 0)
	csvrdr := csv.NewReader(r)
	// csvrdr.Read() // skip 1 line
	for {
		rec, err := csvrdr.Read()
		if errors.Is(err, io.EOF) {
			break
		}
		if err != nil {
			return nil, err
		}
		result = append(result, rec)
	}
	return result, nil
}

func (t *TableGraph) setContentTable(msg string) {
	data, err := parseCSV(strings.NewReader(msg))
	if err != nil {
		panic(err)
	}
	tableData := genericTableData{
		columns: data[0],
		rows:    data[1:],
	}

	t.table.SetContent(tableData)
}

func (t *TableGraph) setContentGraph() {
	row := t.table.SelectedRow()
	t.viewport.SetContent(t.plotGraph(strSliceToNumbers(row[2:]), row[0]))
}
