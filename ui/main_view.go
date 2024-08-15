package ui

import (
	"github.com/charmbracelet/bubbles/spinner"
)

type mainView struct {
	ContentModel

	id          int
	ready       bool
	isDataReady bool
	name        string
	spinner     spinner.Model
}

func newMainView(id int, name string, contentModel ContentModel) *mainView {
	return &mainView{
		id:           id,
		name:         name,
		ContentModel: contentModel,
		spinner:      newSpinner(),
		isDataReady:  false,
	}
}

func (m *mainView) SetContent(c content) {
	m.ContentModel.SetContent(c)
}
