package ui

import (
	"fmt"

	tea "github.com/charmbracelet/bubbletea"
)

type size tea.WindowSizeMsg

func (s size) String() string {
	return fmt.Sprintf("w=%d, h=%d", s.Width, s.Height)
}
