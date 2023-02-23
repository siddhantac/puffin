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
	style lipgloss.Style
}

func NewMainSection() MainSection {
	return MainSection{
		Table: buildTable(columns(20)),
		keys:  Keys,
		style: lipgloss.NewStyle().
			BorderBottom(true).
			BorderTop(true).
			// BorderRight(true).
			MarginBottom(2).
			PaddingBottom(2).
			BorderStyle(lipgloss.NormalBorder()).
			BorderForeground(lipgloss.Color("60")),
	}
}

func (m MainSection) Init() tea.Cmd {
	return nil
}

func (m MainSection) Update(msg tea.Msg) (MainSection, tea.Cmd) {
	switch msg := msg.(type) {
	case tea.WindowSizeMsg:
		m.Table.SetHeight(msg.Height / 2)
		m.Table.SetWidth(msg.Width)
	case tea.KeyMsg:
		switch {
		case key.Matches(msg, m.keys.Up):
			m.Table.MoveUp(1)
		case key.Matches(msg, m.keys.Down):
			m.Table.MoveDown(1)
		}

	case transactionsData: // set table data when it changes
		m.Table.SetRows(msg)
	}

	return m, nil
}

func (m MainSection) View() string {
	return m.style.Render(m.Table.View())
}

func columns(screenWidth int) []table.Column {
	return []table.Column{
		{Title: "txnidx", Width: 18},
		{Title: "date", Width: 30},
		{Title: "description", Width: 50},
		{Title: "account", Width: 50},
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

	// s.Cell = lipgloss.NewStyle().
	// 	MarginBottom(1)
	// 	BorderStyle(lipgloss.NormalBorder()).
	// 	BorderBottom(true).
	// 	BorderForeground(lipgloss.Color("66"))

	s.Selected = lipgloss.NewStyle().
		Background(lipgloss.Color("108"))

	t.SetStyles(s)

	return t
}
