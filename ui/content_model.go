package ui

import tea "github.com/charmbracelet/bubbletea"

// ContentModel extends the tea.Model interface with
// methods which make it possible to dynamically update
// the content and set the model to an "unready" status.
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
