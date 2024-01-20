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

	if err := tea.NewProgram(newModel(hl)).Start(); err != nil {
		fmt.Println("Error running program:", err)
		os.Exit(1)
	}
}
