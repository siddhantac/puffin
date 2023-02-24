package v2

import (
	"puffin/hledger"
	"strings"

	"github.com/charmbracelet/bubbles/table"
	"github.com/charmbracelet/bubbles/viewport"
	tea "github.com/charmbracelet/bubbletea"
	"github.com/charmbracelet/lipgloss"
)

type Sidebar struct {
	viewport     viewport.Model
	style        lipgloss.Style
	screenHeight int
	content      string
	table        table.Model
	hlcmd        HledgerCmd
}

func NewSidebar(hlcmd HledgerCmd) Sidebar {
	return Sidebar{
		viewport:     viewport.New(0, 0),
		screenHeight: 5,
		hlcmd:        hlcmd,
		style: lipgloss.NewStyle().
			PaddingLeft(2).
			PaddingTop(2).
			MarginLeft(2).
			BorderStyle(lipgloss.NormalBorder()).
			BorderLeft(true).
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
		s.screenHeight = msg.Height
		s.viewport.Height = msg.Height
		s.viewport.Width = msg.Width
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
		m := make([]string, 0)
		for _, x := range msg {
			xx := strings.Join(x, " ")
			m = append(m, xx)
		}
		s.content = strings.Join(m, "\n")
		s.viewport.SetContent(s.content)
	}

	return s, nil
}

func (s Sidebar) View() string {
	return s.style.Render(s.content)
}

func (s Sidebar) buildTable(columns []table.Column) table.Model {
	t := table.New(
		table.WithColumns(columns),
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

	style.Selected = lipgloss.NewStyle().
		Background(lipgloss.Color("108"))

	t.SetStyles(style)

	return t
}
