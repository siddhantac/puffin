package ui

import (
	"github.com/charmbracelet/bubbles/key"
	"github.com/charmbracelet/bubbles/table"
	tea "github.com/charmbracelet/bubbletea"
	"github.com/charmbracelet/lipgloss"
)

func balanceColumns() []table.Column {
	return []table.Column{
		{Title: "name", Width: 25},
		{Title: "amount", Width: 15},
	}
}

func percent(number, percentage int) int {
	return (percentage * number) / 100
}

func newTable(columns []table.Column) table.Model {
	t := table.New(
		table.WithColumns(columns),
		table.WithKeyMap(table.DefaultKeyMap()),
		table.WithFocused(true),
	)

	s := table.DefaultStyles()
	s.Header = s.Header.
		BorderStyle(lipgloss.NormalBorder()).
		BorderForeground(theme.SecondaryForeground).
		BorderTop(true).
		BorderBottom(true).
		Bold(false)
	s.Selected = s.Selected.
		Foreground(theme.PrimaryForeground).
		Background(theme.Accent).
		Bold(false)
	t.SetStyles(s)

	return t
}

type Table struct {
	table.Model
	columns func() []table.Column
}

func NewTable(columns func() []table.Column) *Table {
	return &Table{
		columns: columns,
		Model:   newTable(columns()),
	}
}

func (t *Table) Init() tea.Cmd {
	return nil
}

func (t *Table) Update(msg tea.Msg) (tea.Model, tea.Cmd) {
	switch msg := msg.(type) {
	case tea.WindowSizeMsg:
		// t.SetWidth(msg.Width)
		t.Model = newTable(t.columns())

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
