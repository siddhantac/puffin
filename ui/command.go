package ui

import (
	"io"
	"puffin/accounting"

	tea "github.com/charmbracelet/bubbletea"
)

func runCommand(cmd string) tea.Cmd {
	return func() tea.Msg {
		buf, err := accounting.RunCommand(cmd)
		if err != nil {
			return err
		}
		b, err := io.ReadAll(buf)
		if err != nil {
			return err.Error()
		}
		return string(b)
	}
}
