package ui

import (
	"fmt"
	"log"

	"github.com/charmbracelet/bubbles/spinner"
	tea "github.com/charmbracelet/bubbletea"
	"github.com/charmbracelet/lipgloss"
)

type mainView struct {
	id          int
	isDataReady bool
	name        string

	ContentModel
	spinner  spinner.Model
	errorMsg string
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
	if c.err != nil {
		m.errorMsg = c.err.Error()
		return
	}
	m.errorMsg = ""
	m.ContentModel.SetContent(c)
}

func (m *mainView) View() string {
	if m.errorMsg != "" {
		log.Printf("%v: errmsg set", m.name)
		msg := fmt.Sprintf("⚠️ Error\n\n\t%s", m.errorMsg)
		return lipgloss.NewStyle().Foreground(theme.Accent).Render(msg)
	}

	if !m.ContentModel.IsReady() {
		return m.spinner.View()
	}

	return m.ContentModel.View()
}
