package ui

import (
	"github.com/charmbracelet/bubbles/spinner"
	tea "github.com/charmbracelet/bubbletea"
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

func (m *mainView) Init() tea.Cmd { return m.spinner.Tick }

func (m *mainView) Update(msg tea.Msg) (tea.Model, tea.Cmd) {
	switch msg := msg.(type) {
	case spinner.TickMsg:
		var cmd tea.Cmd
		if !m.ContentModel.IsReady() {
			m.spinner, cmd = m.spinner.Update(msg)
		}
		return m, cmd
	default:
		return m.ContentModel.Update(msg)
	}
}

func (m *mainView) SetContent(c content) {
	m.ContentModel.SetContent(c)
}

func (m *mainView) View() string {
	if !m.ContentModel.IsReady() {
		return m.spinner.View()
	}
	return m.ContentModel.View()
}
