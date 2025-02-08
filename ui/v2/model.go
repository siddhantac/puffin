package ui

import (
	"github.com/charmbracelet/bubbles/table"
	"github.com/charmbracelet/bubbles/viewport"
	tea "github.com/charmbracelet/bubbletea"
)

type Table struct {
	table.Model
}

func (Table) Init() tea.Cmd {
	return nil
}
func (t Table) Update(msg tea.Msg) (tea.Model, tea.Cmd) { return t, nil }

type Viewport struct {
	viewport.Model
}

func (Viewport) Init() tea.Cmd {
	return nil
}
func (v Viewport) Update(msg tea.Msg) (tea.Model, tea.Cmd) { return v, nil }
