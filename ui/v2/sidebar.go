package v2

import (
	"github.com/charmbracelet/bubbles/viewport"
	tea "github.com/charmbracelet/bubbletea"
	"github.com/charmbracelet/lipgloss"
)

type Sidebar struct {
	viewport viewport.Model
	style    lipgloss.Style
}

func NewSidebar() Sidebar {
	return Sidebar{
		viewport: viewport.New(0, 0),
		style: lipgloss.NewStyle().
			MarginLeft(2).
			MarginTop(2),
	}
}

func (s Sidebar) Init() tea.Cmd {
	return nil
}

func (s Sidebar) Update(msg tea.Msg) (Sidebar, tea.Cmd) {
	switch msg := msg.(type) {
	case tea.WindowSizeMsg:
		s.viewport.Height = msg.Height / 2
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
	}

	return s, nil
}

func (s Sidebar) View() string {
	text := "hello world\nlorem ipsum\ndolor sit amet\nlorem ipsum\ndolor site amet\n"
	return s.style.Render(text)
}
