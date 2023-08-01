package ui

import (
	"puffin/logger"

	"github.com/charmbracelet/bubbles/key"
	"github.com/charmbracelet/bubbles/table"
	tea "github.com/charmbracelet/bubbletea"
)

type Table interface {
	tea.Model
	SetHeight(int)
	SetWidth(int)
	MoveUp(int)
	MoveDown(int)
}

type TableWrapper struct {
	Table
}

func NewTableWrapper(tableCols Table) *TableWrapper {
	return &TableWrapper{
		Table: tableCols,
	}
}

func (t *TableWrapper) Init() tea.Cmd {
	return t.Table.Init()
}

func (t *TableWrapper) Update(msg tea.Msg) (tea.Model, tea.Cmd) {
	switch msg := msg.(type) {
	case tea.WindowSizeMsg:
		tableWidth := percent(msg.Width, 100)
		tableHeight := msg.Height - 9
		logger.Logf("height: %v, tableHeight: %v", msg.Height, tableHeight)

		t.Table.SetWidth(tableWidth)
		t.Table.SetHeight(tableHeight)

	case tea.KeyMsg:
		switch {
		case key.Matches(msg, allKeys.Up):
			t.Table.MoveUp(1)
		case key.Matches(msg, allKeys.Down):
			t.Table.MoveDown(1)
		}
	default:
		t.Table.Update(msg)
	}

	return t, nil
}

func (t *TableWrapper) View() string {
	return t.Table.View()
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

func newTableWithBorders(columns []table.Column) *table.Model {
	tbl := table.New(
		table.WithColumns(columns),
		table.WithKeyMap(table.DefaultKeyMap()),
		table.WithFocused(true),
	)

	tbl.SetStyles(tableStyleWithBorders())
	return &tbl
}
