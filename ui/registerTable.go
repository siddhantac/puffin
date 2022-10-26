package ui

import (
	"hledger/hledger"

	"github.com/charmbracelet/bubbles/table"
	tea "github.com/charmbracelet/bubbletea"
)

var models []tea.Model

type modelType int

const (
	registerTableModel modelType = iota
	balanceTableModel
	filterFormModel
)

type registerTable struct {
	table    table.Model
	hlcmd    HledgerCmd
	quitting bool
}

func newRegisterTableModel(hl hledger.Hledger) *registerTable {
	return &registerTable{
		hlcmd: NewHledgerCmd(hl),
		table: buildTable(registerColumns()),
	}
}

func (m *registerTable) Init() tea.Cmd {
	return m.hlcmd.register("expenses")
}

func (m *registerTable) Update(msg tea.Msg) (tea.Model, tea.Cmd) {
	switch msg := msg.(type) {
	case tea.KeyMsg:
		switch msg.String() {
		case "q", "ctrl+c":
			m.quitting = true
			return m, tea.Quit
		case "/":
			return m, m.hlcmd.register("dbs")
		case ".":
			return m, tea.Printf("wait ah, switching not built yet")
		case "r": // TODO
			return m, m.hlcmd.register("expenses")
		}
	case tableData: // set table data when it changes
		m.table.SetRows(msg.rows)
	}

	return m, nil
}

func (m *registerTable) View() string {
	if m.quitting {
		return ""
	}

	return helpString() + baseStyle.Render(m.table.View()) + "\n"
}
