package ui

import (
	"fmt"
	"log"
	"strings"

	"github.com/charmbracelet/bubbles/table"
	tea "github.com/charmbracelet/bubbletea"
	"github.com/charmbracelet/lipgloss"
	"github.com/siddhantac/hledger"
)

type TableData interface {
	Columns() table.Row
	Rows() []table.Row
}

type Table struct {
	*table.Model
	name              string
	size              size
	columnPercentages []int
	columns           []table.Column
	isDataReady       bool
	id                int
	cmd               func(int, hledger.Options) content
	locked            bool
	cmdType           cmdType
	dataTransformers  []dataTransformer
	numRows           int
}

func newTable(name string, columnPercentages []int, id int, cmd func(int, hledger.Options) content, locked bool, cmdType cmdType, dataTransformers []dataTransformer) *Table {
	return &Table{
		id:                id,
		cmd:               cmd,
		locked:            locked,
		name:              name,
		cmdType:           cmdType,
		columnPercentages: columnPercentages,
		Model:             &table.Model{},
		dataTransformers:  dataTransformers,
	}
}

func (t *Table) log(msg string) {
	log.Printf("%s(%d): %s", t.name, t.id, msg)
}

func (t *Table) Type() cmdType { return t.cmdType }
func (t *Table) Locked() bool  { return t.locked }
func (t *Table) IsReady() bool { return t.isDataReady }
func (t *Table) SetUnready() {
	t.isDataReady = false
	t.log("unready")
}
func (t *Table) NumRows() int { return t.numRows }

func (t *Table) Run(options hledger.Options) tea.Cmd {
	return func() tea.Msg {
		return t.cmd(t.id, options)
	}
}

func (t *Table) SetContent(gc content) {
	if gc.id != t.id {
		return
	}

	t.isDataReady = false
	data, err := parseCSV(strings.NewReader(gc.msg))
	if err != nil {
		t.log(fmt.Sprintf("csv parse error: %v", err))
		return
	}

	t.SetColumns(data[0])

	rows := data[1:]

	for _, dt := range t.dataTransformers {
		if err := dt.Transform(rows); err != nil {
			t.log(fmt.Sprintf("data transform error: %v", err))
		}
	}
	t.SetRows(rows)
	t.numRows = len(rows)

	t.isDataReady = true
}

func (t *Table) Init() tea.Cmd {
	t.SetUnready()
	return nil
}

func (t *Table) Update(msg tea.Msg) (tea.Model, tea.Cmd) {
	var cmd tea.Cmd
	switch msg := msg.(type) {
	case tea.WindowSizeMsg:
		t.size = size{
			Width:  percent(msg.Width, 99),
			Height: msg.Height - 4, // 3 for header row, 1 for the border at the bottom
		}
		t.log(fmt.Sprintf("tableSize: %s", t.size))

		t.Model.SetWidth(t.size.Width)
		t.Model.SetHeight(t.size.Height)

	case tea.KeyMsg:
		switch msg.String() {
		// case key.Matches(msg, allKeys.ScrollUp):
		case "K":
			t.Model.MoveUp(1)
			// case key.Matches(msg, allKeys.ScrollDown):
		case "J":
			t.Model.MoveDown(1)
		}
	default:
		_, cmd = t.Model.Update(msg)
	}

	return t, cmd
}

func (t *Table) View() string {
	s := lipgloss.NewStyle().
		BorderStyle(lipgloss.NormalBorder()).
		BorderForeground(lipgloss.Color("240")).
		BorderBottom(true)
	return s.Render(t.Model.View())
}

func percent(number, percentage int) int {
	return (percentage * number) / 100
}

func newDefaultTable(columns []table.Column) *table.Model {
	tbl := table.New(
		table.WithColumns(columns),
		table.WithKeyMap(table.DefaultKeyMap()),
		table.WithFocused(true),
		// table.WithHeight(22),
	)

	tbl.SetStyles(getTableStyle())
	return &tbl
}

func (t *Table) SetColumns(firstRow table.Row) {
	// if len(t.columnPercentages) == 0 {
	t.columnPercentages = make([]int, 0, len(firstRow))
	for range firstRow {
		t.columnPercentages = append(t.columnPercentages, 100/len(firstRow))
	}
	// }
	// if len(t.columnPercentages) != len(firstRow) {
	// 	panic(fmt.Sprintf("length not equal: expected=%d, got=%d", len(t.columnPercentages), len(firstRow)))
	// }

	cols := make([]table.Column, 0, len(firstRow))
	for i, row := range firstRow {
		c := table.Column{Title: row, Width: percent(t.size.Width, t.columnPercentages[i])}
		cols = append(cols, c)
	}

	if len(cols) != len(t.columns) {
		t.columns = cols
		t.Model = newDefaultTable(cols)
		t.Model.SetHeight(t.size.Height)
		t.Model.SetWidth(t.size.Width)
	}
}
