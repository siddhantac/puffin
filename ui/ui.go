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

var Version string

func Start(hlcmd accounting.HledgerCmd, config Config, isDebug bool) {
	if isDebug {
		f, err := tea.LogToFile("puffin.log", "debug")
		if err != nil {
			panic(err)
		}
		defer f.Close()
	} else {
		log.SetOutput(io.Discard)
	}

	log.Printf("init puffin %s", Version)
	p := tea.NewProgram(newModel(hlcmd, config))
	if _, err := p.Run(); err != nil {
		fmt.Println("Error running program:", err)
		os.Exit(1)
	}
}
