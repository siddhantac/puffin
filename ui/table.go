package ui

import (
	"puffin/accounting"
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

type TableWrapper struct {
	*table.Model
	width             int
	height            int
	columnPercentages []int
	columns           []table.Column
}

func NewTableWrapper(columnPercentages []int) *TableWrapper {
	return &TableWrapper{
		columnPercentages: columnPercentages,
		Model:             &table.Model{},
	}
}

func (t *TableWrapper) SetContent(msg tea.Msg) {
	td, ok := msg.(TableData)
	if !ok {
		return
	}
	t.SetColumns(td.Columns())
	t.SetRows(td.Rows())
}
func (t *TableWrapper) IsReady() bool { return true }

func (t *TableWrapper) Init() tea.Cmd {
	return nil
}

func (t *TableWrapper) Update(msg tea.Msg) (tea.Model, tea.Cmd) {
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
	case accounting.RegisterData: // set table data when it changes
		t.SetColumns(msg.Columns())
		t.Model.SetRows(msg.Rows())
	default:
		t.Model.Update(msg)
	}

	return t, nil
}

func (t *TableWrapper) View() string {
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

func (t *TableWrapper) SetWidth(width int) {
	t.width = width
	t.Model.SetWidth(width)
}

func (t *TableWrapper) SetHeight(height int) {
	t.height = height
	t.Model.SetHeight(height)
}

func (t *TableWrapper) SetColumns(firstRow table.Row) {
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
