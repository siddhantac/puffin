package ui

import (
	"puffin/logger"

	"github.com/charmbracelet/bubbles/key"
	"github.com/charmbracelet/bubbles/table"
	tea "github.com/charmbracelet/bubbletea"
)

// type Table interface {
// 	tea.Model
// 	SetHeight(int)
// 	SetWidth(int)
// 	MoveUp(int)
// 	MoveDown(int)
// }

type TableData interface {
	Columns() table.Row
	Rows() []table.Row
}

type Table struct {
	*table.Model
	width             int
	height            int
	columnPercentages []int
	columns           []table.Column
	isDataReady       bool
}

func NewTable(columnPercentages []int) *Table {
	return &Table{
		columnPercentages: columnPercentages,
		Model:             &table.Model{},
	}
}

func (t *Table) SetContent(msg tea.Msg) {
	td, ok := msg.(TableData)
	if !ok {
		return
	}
	t.SetColumns(td.Columns())
	t.SetRows(td.Rows())
	t.isDataReady = true
}

func (t *Table) IsReady() bool { return t.isDataReady }

func (t *Table) Init() tea.Cmd {
	return nil
}

func (t *Table) Update(msg tea.Msg) (tea.Model, tea.Cmd) {
	switch msg := msg.(type) {
	case tea.WindowSizeMsg:
		tableWidth := percent(msg.Width, 100)
		tableHeight := msg.Height - 9
		logger.Logf("height: %v, tableHeight: %v", msg.Height, tableHeight)

		t.SetWidth(tableWidth)
		t.SetHeight(tableHeight)

	case tea.KeyMsg:
		switch {
		case key.Matches(msg, allKeys.Up):
			t.MoveUp(1)
		case key.Matches(msg, allKeys.Down):
			t.MoveDown(1)
		}
	default:
		t.Model.Update(msg)
	}

	return t, nil
}

func (t *Table) View() string {
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

func (t *Table) SetHeight(height int) {
	t.height = height
	t.Model.SetHeight(height)
}

func (t *Table) SetColumns(firstRow table.Row) {
	if len(t.columnPercentages) != len(firstRow) {
		panic("length not equal")
	}

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

func (t *Table) SetUnready() { t.isDataReady = false }
