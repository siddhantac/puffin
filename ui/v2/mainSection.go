package v2

import (
	"github.com/charmbracelet/bubbles/key"
	"github.com/charmbracelet/bubbles/table"
	tea "github.com/charmbracelet/bubbletea"
	"github.com/charmbracelet/lipgloss"
)

type MainSection struct {
	Table table.Model
	keys  KeyMap
}

func NewMainSection() MainSection {
	return MainSection{
		Table: buildTable(columns(20)),
		keys:  Keys,
	}
}

func (m MainSection) Init() tea.Cmd {
	return nil
}

func (m MainSection) Update(msg tea.Msg) (MainSection, tea.Cmd) {
	switch msg := msg.(type) {
	case tea.WindowSizeMsg:
		w := msg.Width / 4
		m.Table.SetWidth(w)
	case tea.KeyMsg:
		switch {
		case key.Matches(msg, m.keys.Quit):
			return m, tea.Quit
		case key.Matches(msg, m.keys.Up):
			m.Table.MoveUp(1)
		case key.Matches(msg, m.keys.Down):
			m.Table.MoveDown(1)
		}
	}

	return m, nil
}

func (m MainSection) View() string {
	styles := lipgloss.NewStyle().
		BorderBottom(true).
		BorderTop(true).
		BorderStyle(lipgloss.NormalBorder()).
		BorderForeground(lipgloss.Color("60"))

	return styles.Render(m.Table.View())
}

func columns(screenWidth int) []table.Column {
	return []table.Column{
		{Title: "txnidx", Width: 8},
		{Title: "date", Width: 10},
		{Title: "description", Width: 30},
		{Title: "account", Width: 30},
		{Title: "amount", Width: 12},
	}
}

func buildTable(columns []table.Column) table.Model {
	t := table.New(
		table.WithColumns(columns),
		table.WithKeyMap(table.DefaultKeyMap()),
		table.WithFocused(true),
	)

	s := table.DefaultStyles()
	s.Header = s.Header.
		BorderStyle(lipgloss.NormalBorder()).
		BorderForeground(lipgloss.Color("240")).
		BorderBottom(true).
		Bold(false)

	s.Cell = lipgloss.NewStyle().
		Padding(0, 0)

	s.Selected = s.Cell.Copy().
		Foreground(lipgloss.Color("229")).
		Background(lipgloss.Color("8")).
		Bold(false)

	t.SetStyles(s)

	row := []string{"a", "b", "e", "d", "e"}
	t.SetRows([]table.Row{row, row, row})

	return t
}
