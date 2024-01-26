package ui

import tea "github.com/charmbracelet/bubbletea"

type ContentModel interface {
	tea.Model
	IsReady() bool
	SetContent(tea.Msg)
}
