package v2

import (
	"fmt"
	"strings"

	"github.com/charmbracelet/bubbles/viewport"
	tea "github.com/charmbracelet/bubbletea"
	"github.com/charmbracelet/lipgloss"
)

type Sidebar struct {
	viewport     viewport.Model
	style        lipgloss.Style
	screenHeight int
}

func NewSidebar() Sidebar {
	return Sidebar{
		viewport:     viewport.New(0, 0),
		screenHeight: 5,
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
	return nil
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
	}

	return s, nil
}

func (s Sidebar) View() string {
	text := strings.Repeat("\n", s.screenHeight/3)
	text = fmt.Sprintf("%d %s%s", s.screenHeight, "hello world\nlorem ipsum\ndolor sit amet\nlorem ipsum\ndolor site amet\n", text)
	return s.style.Render(text)
}
