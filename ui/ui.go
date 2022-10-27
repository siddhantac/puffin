package ui

import (
	"fmt"
	"hledger/hledger"
	"os"

	tea "github.com/charmbracelet/bubbletea"
)

var models []tea.Model

type modelType int

const (
	registerTableModel modelType = iota
	balanceTableModel
	filterFormModel
)

type UI struct {
	hl hledger.Hledger
}

func New(hl hledger.Hledger) UI {
	return UI{hl: hl}
}

func (ui UI) CreateTable() {
	models = []tea.Model{
		newModel(ui.hl),
		// newBalanceTableModel(ui.hl),
		// newFilterForm(registerTableModel),
	}

	if os.Getenv("DEBUG") != "" {
		f, err := tea.LogToFile("debug.log", "debug")
		if err != nil {
			panic(err)
		}
		defer f.Close()
	}

	if err := tea.NewProgram(models[registerTableModel]).Start(); err != nil {
		fmt.Println("Error running program:", err)
		os.Exit(1)
	}
}
