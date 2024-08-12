package ui

import (
	"log"

	"github.com/charmbracelet/bubbles/spinner"
	"github.com/charmbracelet/bubbles/table"
	tea "github.com/charmbracelet/bubbletea"
)

type TableData interface {
	Columns() table.Row
	Rows() []table.Row
}

type Table struct {
	*table.Model
	name              string
	width             int
	height            int
	columnPercentages []int
	columns           []table.Column
	isDataReady       bool
	spinner           spinner.Model
}

func newTable(name string, columnPercentages []int) *Table {
	return &Table{
		name:              name,
		columnPercentages: columnPercentages,
		Model:             &table.Model{},
		spinner:           newSpinner(),
	}
}

func (t *Table) IsReady() bool { return t.isDataReady }

func (t *Table) SetUnready() { t.isDataReady = false }

func (t *Table) SetContent(msg tea.Msg) {
	td, ok := msg.(TableData)
	if !ok {
		return
	}
	t.SetColumns(td.Columns())
	t.SetRows(td.Rows())
	t.isDataReady = true
}

func (t *Table) Init() tea.Cmd { return t.spinner.Tick }

func (t *Table) Update(msg tea.Msg) (tea.Model, tea.Cmd) {
	var cmd tea.Cmd
	switch msg := msg.(type) {
	case spinner.TickMsg:
		if !t.isDataReady {
			t.spinner, cmd = t.spinner.Update(msg)
		}
	case tea.WindowSizeMsg:
		tableWidth := percent(msg.Width, 100)
		tableHeight := msg.Height - 3
		log.Printf("table(%s): height=%v, tableHeight=%v", t.name, msg.Height, tableHeight)

		t.SetWidth(tableWidth)
		t.height = tableHeight
		t.Model.SetHeight(tableHeight)

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
	if !t.isDataReady {
		return t.spinner.View()
	}
	return t.Model.View()
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

func (t *Table) SetWidth(width int) {
	t.width = width
	t.Model.SetWidth(width)
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
		c := table.Column{Title: row, Width: percent(t.width, t.columnPercentages[i])}
		cols = append(cols, c)
	}

	if len(cols) != len(t.columns) {
		t.columns = cols
		t.Model = newDefaultTable(cols)
		t.Model.SetHeight(t.height)
		t.Model.SetWidth(t.width)
	}
}
