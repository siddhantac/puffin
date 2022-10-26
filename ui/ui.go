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
	models = []tea.Model{
		newRegisterTableModel(ui.hl),
		newBalanceTableModel(ui.hl),
	}

	if err := tea.NewProgram(models[balanceTableModel]).Start(); err != nil {
		fmt.Println("Error running program:", err)
		os.Exit(1)
	}
}

func helpString() string {
	return fmt.Sprintf("'q': quit\n'v': change view\n'/': filter\n'r': refresh\n")
}
