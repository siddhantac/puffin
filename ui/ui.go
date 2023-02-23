package ui

import (
	"fmt"
	"os"
	"puffin/hledger"
	v2 "puffin/ui/v2"

	tea "github.com/charmbracelet/bubbletea"
)

type UI struct {
	hldgr hledger.Hledger
}

func New(hl hledger.Hledger) UI {
	return UI{hldgr: hl}
}

func (ui UI) Start() {
	if os.Getenv("DEBUG") != "" {
		f, err := tea.LogToFile("debug.log", "debug")
		if err != nil {
			panic(err)
		}
		defer f.Close()
	}

	hlcmd := v2.NewHledgerCmd(ui.hldgr)

	// if err := tea.NewProgram(newModel(ui.hl), tea.WithAltScreen()).Start(); err != nil {
	if err := tea.NewProgram(v2.NewModel(hlcmd), tea.WithAltScreen()).Start(); err != nil {
		fmt.Println("Error running program:", err)
		os.Exit(1)
	}
}
