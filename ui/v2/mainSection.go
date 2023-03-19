package v2

import (
	"puffin/hledger"

	"github.com/charmbracelet/bubbles/key"
	"github.com/charmbracelet/bubbles/table"
	tea "github.com/charmbracelet/bubbletea"
	"github.com/charmbracelet/lipgloss"
)

type MainSection struct {
	Table table.Model
	keys  KeyMap
	style lipgloss.Style
	width int
	hlcmd HledgerCmd
}

func NewMainSection(hlcmd HledgerCmd) MainSection {
	return MainSection{
		hlcmd: hlcmd,
		keys:  Keys,
		style: lipgloss.NewStyle().
			Border(lipgloss.NormalBorder()).
			BorderStyle(lipgloss.NormalBorder()).
			BorderForeground(lipgloss.Color("60")),
	}
}

func (m MainSection) Init() tea.Cmd {
	simpleAccountFilter := hledger.NewSimpleAccountFilter()
	return m.hlcmd.register(true, simpleAccountFilter)
}

func (m MainSection) Update(msg tea.Msg) (MainSection, tea.Cmd) {
	switch msg := msg.(type) {
	case tea.WindowSizeMsg:
		m.width = msg.Width * 75 / 100
		m.Table.SetHeight(msg.Height / 2)
		m.Table.SetWidth(m.width)
		m.Table = buildTable(columns(m.width))
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
		{Title: "id", Width: screenWidth * 10 / 100},
		{Title: "date", Width: screenWidth * 10 / 100},
		{Title: "description", Width: screenWidth * 25 / 100},
		{Title: "account", Width: screenWidth * 30 / 100},
		{Title: "amount", Width: screenWidth * 10 / 100},
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
		Foreground(lipgloss.Color("108")).
		Bold(true)

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
