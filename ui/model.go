package ui

import (
	"fmt"
	"hledger/hledger"

	"github.com/charmbracelet/bubbles/table"
	tea "github.com/charmbracelet/bubbletea"
)

type model struct {
	registerTable   table.Model
	balanceTable    table.Model
	hlcmd           HledgerCmd
	showBalanceView bool
	quitting        bool
}

func newModel(hl hledger.Hledger) *model {
	return &model{
		hlcmd:           NewHledgerCmd(hl),
		showBalanceView: false,
	}
}

func (m *model) Init() tea.Cmd {
	m.registerTable = buildTable(registerColumns())
	m.balanceTable = buildTable(balanceColumns())
	return m.hlcmd.register("expenses")
}

func (m *model) Update(msg tea.Msg) (tea.Model, tea.Cmd) {
	var cmd tea.Cmd
	switch msg := msg.(type) {
	case tea.KeyMsg:
		switch msg.String() {
		case "esc":
			if m.registerTable.Focused() {
				m.registerTable.Blur()
			} else {
				m.registerTable.Focus()
			}
		case "q", "ctrl+c":
			m.quitting = true
			return m, tea.Quit
		case "enter":
			return m, tea.Batch(
				tea.Printf("Let's go to %s!", m.registerTable.SelectedRow()[1]),
			)
		case "/":
			return m, m.hlcmd.register("dbs_twisha")
		case "v":
			if m.showBalanceView {
				m.showBalanceView = false
				return m, m.hlcmd.register("expenses")
			} else {
				m.showBalanceView = true
				return m, m.hlcmd.balance("")
			}
		}
	case tableData:
		if m.showBalanceView {
			m.balanceTable.SetRows(msg.rows)
		} else {
			m.registerTable.SetRows(msg.rows)
		}
		return m, nil
	}
	m.registerTable, cmd = m.registerTable.Update(msg)
	return m, cmd
}

func (m *model) View() string {
	if m.quitting {
		return ""
	}

	helpString := fmt.Sprintf("'q': quit\n'v': change view\n'/': filter\n")
	var tbl table.Model
	if m.showBalanceView {
		tbl = m.balanceTable
	} else {
		tbl = m.registerTable
	}

	return helpString + baseStyle.Render(tbl.View()) + "\n"
}
