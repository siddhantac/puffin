package ui

import (
	"hledger/hledger"
	"sync"

	"github.com/charmbracelet/bubbles/table"
	tea "github.com/charmbracelet/bubbletea"
)

type balanceTable struct {
	table    table.Model
	hlcmd    HledgerCmd
	quitting bool
}

func newBalanceTableModel(hl hledger.Hledger) *balanceTable {
	return &balanceTable{
		hlcmd: NewHledgerCmd(hl),
		table: buildTable(balanceColumns()),
	}
}

func (m *balanceTable) Init() tea.Cmd {
	return m.hlcmd.balance("")
}

var once sync.Once

func (m *balanceTable) Update(msg tea.Msg) (tea.Model, tea.Cmd) {
	switch msg := msg.(type) {
	case tea.KeyMsg:
		switch msg.String() {
		case "q", "ctrl+c":
			m.quitting = true
			return m, tea.Quit
		case "/":
			models[balanceTableModel] = m // save current state
			models[filterFormModel] = newFilterForm(balanceTableModel)
			return models[filterFormModel].Update(nil)
		case ".":
			return models[registerTableModel], nil
		case "r": // TODO
			return m, m.hlcmd.balance("")
		}
	case tableData:
		m.table.SetRows(msg.rows)
	}

	return m, nil
}

func (m *balanceTable) View() string {
	if m.quitting {
		return ""
	}

	return helpString() + baseStyle.Render(m.table.View()) + "\n"
}
