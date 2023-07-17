package ui

import (
	"github.com/charmbracelet/bubbles/key"
	tea "github.com/charmbracelet/bubbletea"
)

type TableColumns interface {
	SetColumns(width int)
	SetRows(interface{})
	SetHeight(int)
	SetWidth(int)
	MoveUp(int)
	MoveDown(int)
}

type TableCustom struct {
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

		t.TableColumns.SetColumns(tableWidth)

		t.TableColumns.SetWidth(tableWidth)
		t.TableColumns.SetHeight(tableHeight)

	case tea.KeyMsg:
		switch {
		case key.Matches(msg, allKeys.Up):
			t.TableColumns.MoveUp(1)
		case key.Matches(msg, allKeys.Down):
			t.TableColumns.MoveDown(1)
		}
	}

	return t, nil
}

func (t *TableCustom) View() string {
	return t.Model.View()
}
