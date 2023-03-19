package v2

import (
	"puffin/hledger"

	"github.com/charmbracelet/bubbles/table"
	tea "github.com/charmbracelet/bubbletea"
	"github.com/charmbracelet/lipgloss"
)

type Sidebar struct {
	// viewport     viewport.Model
	style        lipgloss.Style
	screenHeight int
	content      string
	table        table.Model
	hlcmd        HledgerCmd
	width        int
}

func NewSidebar(hlcmd HledgerCmd) Sidebar {
	return Sidebar{
		// viewport:     viewport.New(0, 0),
		screenHeight: 5,
		hlcmd:        hlcmd,
		style: lipgloss.NewStyle().
			PaddingLeft(0).
			// PaddingTop(4).
			MarginLeft(2).
			BorderStyle(lipgloss.NormalBorder()).
			BorderForeground(lipgloss.Color("60")),
	}
}

func (s Sidebar) Init() tea.Cmd {
	return s.hlcmd.balance(
		hledger.NewAccountFilter("assets:bank"),
		hledger.NewAccountDepthFilter().SetDepth(3),
	)
}

func (s Sidebar) Update(msg tea.Msg) (Sidebar, tea.Cmd) {
	switch msg := msg.(type) {
	case tea.WindowSizeMsg:
		s.width = msg.Width * 25 / 100
		s.screenHeight = msg.Height
		// s.viewport.Height = msg.Height
		// s.viewport.Width = s.width
		s.table = s.buildTable()
		// case tea.KeyMsg:
		// 	switch {
		// 	case key.Matches(msg, m.keys.Quit):
		// 		return m, tea.Quit
		// 	case key.Matches(msg, m.keys.Up):
		// 		m.Table.MoveUp(1)
		// 	case key.Matches(msg, m.keys.Down):
		// 		m.Table.MoveDown(1)
		// 	}
	case accountsData: // set table data when it changes
		s.table.SetRows(msg)
		/* m := make([]string, 0)
		for _, x := range msg {
			xx := strings.Join(x, " ")
			m = append(m, xx)
		}
		s.content = strings.Join(m, "\n")
		s.viewport.SetContent(s.content) */
	}

	return s, nil
}

func (s Sidebar) View() string {
	return s.style.Render(s.table.View())
}

func (s Sidebar) columns() []table.Column {
	return []table.Column{
		{Title: "account", Width: s.width * 50 / 100},
		{Title: "amount", Width: s.width * 50 / 100},
	}
}

func (s Sidebar) buildTable() table.Model {
	t := table.New(
		table.WithColumns(s.columns()),
		table.WithKeyMap(table.DefaultKeyMap()),
		table.WithFocused(false),
	)

	style := table.DefaultStyles()
	style.Header = style.Header.
		BorderStyle(lipgloss.NormalBorder()).
		BorderForeground(lipgloss.Color("240")).
		BorderBottom(true).
		Foreground(lipgloss.Color("108")).
		Bold(true)

	style.Selected = lipgloss.NewStyle()
	// Background(lipgloss.Color("108"))

	t.SetStyles(style)

	return t
}
