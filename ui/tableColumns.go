package ui

import (
	"github.com/charmbracelet/bubbles/key"
	"github.com/charmbracelet/bubbles/table"
	tea "github.com/charmbracelet/bubbletea"
)

type TableColumns interface {
	SetColumns(width int, tbl *table.Model)
}

type TableCustom struct {
	*table.Model
	TableColumns
}

func NewTableCustom(tableCols TableColumns) *TableCustom {
	return &TableCustom{
		TableColumns: tableCols,
	}
}

func (t *TableCustom) Init() tea.Cmd {
	return nil
}

func (t *TableCustom) Update(msg tea.Msg) (tea.Model, tea.Cmd) {
	switch msg := msg.(type) {
	case tea.WindowSizeMsg:
		tableWidth := percent(msg.Width, 100)
		tableHeight := percent(msg.Height, 80)

		t.TableColumns.SetColumns(tableWidth, t.Model)

		t.Model.SetWidth(tableWidth)
		t.Model.SetHeight(tableHeight)

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

func (t *TableCustom) View() string {
	return t.Model.View()
}
