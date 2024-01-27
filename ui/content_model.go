package ui

import tea "github.com/charmbracelet/bubbletea"

type ContentModel interface {
	tea.Model
	IsReady() bool
	SetUnready()
	SetContent(tea.Msg)
}

type modelLoading struct{}

func setModelLoading() tea.Msg {
	return modelLoading{}
}
