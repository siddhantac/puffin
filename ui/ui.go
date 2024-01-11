package ui

import (
	"fmt"
	"os"
	"puffin/hledger"

	tea "github.com/charmbracelet/bubbletea"
	hlgo "github.com/siddhantac/hledger"
)

type UI struct {
	hl  hledger.Hledger
	hl2 hlgo.Hledger
}

func New(hl hledger.Hledger, hl2 hlgo.Hledger) UI {
	return UI{hl: hl, hl2: hl2}
}

func (ui UI) Start() {
	if os.Getenv("DEBUG") != "" {
		f, err := tea.LogToFile("debug.log", "debug")
		if err != nil {
			panic(err)
		}
		defer f.Close()
	}

	if err := tea.NewProgram(newModel(ui.hl, ui.hl2)).Start(); err != nil {
		fmt.Println("Error running program:", err)
		os.Exit(1)
	}
}
