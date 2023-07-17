package ui

import (
	"github.com/charmbracelet/bubbles/key"
	"github.com/charmbracelet/bubbles/table"
	tea "github.com/charmbracelet/bubbletea"
)

type TableColumns interface {
	tea.Model
	SetColumns(width int)
	SetRows([]table.Row)
	SetHeight(int)
	SetWidth(int)
	MoveUp(int)
	MoveDown(int)
}

type Table struct {
	TableColumns
}

func NewTable(tableCols TableColumns) *Table {
	return &Table{
		TableColumns: tableCols,
	}
}

func (t *Table) Init() tea.Cmd {
	return t.TableColumns.Init()
}

func (t *Table) Update(msg tea.Msg) (tea.Model, tea.Cmd) {
	switch msg := msg.(type) {
	case tea.WindowSizeMsg:
		tableWidth := percent(msg.Width, 100)
		tableHeight := percent(msg.Height, 80)

		t.TableColumns.SetWidth(tableWidth)
		t.TableColumns.SetHeight(tableHeight)
		t.TableColumns.SetColumns(tableWidth)

	case tea.KeyMsg:
		switch {
		case key.Matches(msg, allKeys.Up):
			t.TableColumns.MoveUp(1)
		case key.Matches(msg, allKeys.Down):
			t.TableColumns.MoveDown(1)
		}
	default:
		t.TableColumns.Update(msg)
	}

	return t, nil
}

func (t *Table) View() string {
	return t.TableColumns.View()
}
