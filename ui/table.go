package ui

import (
	"puffin/logger"

	"github.com/charmbracelet/bubbles/key"
	"github.com/charmbracelet/bubbles/table"
	tea "github.com/charmbracelet/bubbletea"
)

func percent(number, percentage int) int {
	return (percentage * number) / 100
}

func newDefaultTable(columns []table.Column) table.Model {
	tbl := table.New(
		table.WithColumns(columns),
		table.WithKeyMap(table.DefaultKeyMap()),
		table.WithFocused(true),
	)

	tbl.SetStyles(getTableStyle())
	return tbl
}

type ColumnUpdater interface {
	UpdateColumns(int) table.Model
}
type Table struct {
	table.Model
	columns       func(width int) []table.Column
	ColumnUpdater ColumnUpdater
}

func NewTable(width int, columns func(width int) []table.Column) *Table {
	t := &Table{
		columns: columns,
	}
	t.Model = newDefaultTable(columns(width))
	return t
}

func (t *Table) Init() tea.Cmd {
	return nil
}

func (t *Table) Update(msg tea.Msg) (tea.Model, tea.Cmd) {
	switch msg := msg.(type) {
	case tea.WindowSizeMsg:
		tableWidth := percent(msg.Width, 100)
		t.Model.SetWidth(tableWidth)
		t.Model = newDefaultTable(t.columns(tableWidth))

		tableHeight := percent(msg.Height, 80)
		t.Model.SetHeight(tableHeight)
		logger.Logf("setting height: %v", tableHeight)

	case tea.KeyMsg:
		switch {
		case key.Matches(msg, allKeys.Up):
			t.Model.MoveUp(1)
		case key.Matches(msg, allKeys.Down):
			t.Model.MoveDown(1)
		}
	}

	return t, nil
}

func (t *Table) View() string {
	return t.Model.View()
}
