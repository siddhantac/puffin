package ui

import tea "github.com/charmbracelet/bubbletea"

type cancelFilterMsg struct{}

func cancelFilterCmd() tea.Msg {
	return cancelFilterMsg{}
}

type applyFilterMsg struct{}

func applyFilterCmd() tea.Msg {
	return applyFilterMsg{}
}

type activateFilterMsg struct{}

func activateFilterCmd() tea.Msg {
	return activateFilterMsg{}
}
