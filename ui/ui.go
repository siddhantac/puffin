package ui

import (
	"fmt"
	"os"

	tea "github.com/charmbracelet/bubbletea"
)

func Start(hlcmd HledgerCmd) {
	if os.Getenv("DEBUG") != "" {
		f, err := tea.LogToFile("debug.log", "debug")
		if err != nil {
			panic(err)
		}
		defer f.Close()
	}

	if err := tea.NewProgram(newModel(hlcmd)).Start(); err != nil {
		fmt.Println("Error running program:", err)
		os.Exit(1)
	}
}
