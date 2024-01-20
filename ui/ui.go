package ui

import (
	"fmt"
	"os"
	"puffin/hledger"

	tea "github.com/charmbracelet/bubbletea"
)

type UI struct {
}

func New() UI {
	return UI{}
}

func (ui UI) Start(hl hledger.Hledger) {
	if os.Getenv("DEBUG") != "" {
		f, err := tea.LogToFile("debug.log", "debug")
		if err != nil {
			panic(err)
		}
		defer f.Close()
	}

	hlcmd := NewHledgerCmd(hl)
	if err := tea.NewProgram(newModel(hlcmd)).Start(); err != nil {
		fmt.Println("Error running program:", err)
		os.Exit(1)
	}
}
