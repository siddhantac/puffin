package ui

import (
	"fmt"
	"os"
	"puffin/hledger"

	tea "github.com/charmbracelet/bubbletea"
)

type UI struct {
	hl hledger.Hledger
}

func New(hl hledger.Hledger) UI {
	return UI{hl: hl}
}

func (ui UI) Start() {
	if os.Getenv("DEBUG") != "" {
		f, err := tea.LogToFile("debug.log", "debug")
		if err != nil {
			panic(err)
		}
		defer f.Close()
	}

	if err := tea.NewProgram(newModel(ui.hl)).Start(); err != nil {
		fmt.Println("Error running program:", err)
		os.Exit(1)
	}
}
