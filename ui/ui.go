package ui

import (
	"fmt"
	"hledger/hledger"
	"os"

	tea "github.com/charmbracelet/bubbletea"
)

type UI struct {
	hl hledger.Hledger
}

func New(hl hledger.Hledger) UI {
	return UI{hl: hl}
}

func (ui UI) CreateTable() {
	m := newModel(ui.hl)
	m.registerTable = buildTable(initialColumns())

	if err := tea.NewProgram(m).Start(); err != nil {
		fmt.Println("Error running program:", err)
		os.Exit(1)
	}
}
