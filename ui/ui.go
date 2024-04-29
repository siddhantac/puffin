package ui

import (
	"fmt"
	"io"
	"log"
	"os"
	"puffin/accounting"

	tea "github.com/charmbracelet/bubbletea"
)

const (
	footerHeight = 3
)

func Start(hlcmd accounting.HledgerCmd, isDebug bool) {
	if isDebug {
		f, err := tea.LogToFile("puffin.log", "debug")
		if err != nil {
			panic(err)
		}
		defer f.Close()
	} else {
		log.SetOutput(io.Discard)
	}

	log.Printf("init")
	if err := tea.NewProgram(newModel(hlcmd)).Start(); err != nil {
		fmt.Println("Error running program:", err)
		os.Exit(1)
	}
}
