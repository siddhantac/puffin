package ui

import tea "github.com/charmbracelet/bubbletea"

type blurFilterMsg struct{}

func blurFilterCmd() tea.Msg {
	return blurFilterMsg{}
}

type refreshDataMsg struct{}

func refreshDataCmd() tea.Msg {
	return refreshDataMsg{}
}

type focusFilterMsg struct{}

func focusFilterCmd() tea.Msg {
	return focusFilterMsg{}
}
