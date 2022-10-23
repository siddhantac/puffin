package ui

import (
	"fmt"
	"hledger/hledger"

	"github.com/charmbracelet/bubbles/table"
	tea "github.com/charmbracelet/bubbletea"
)

type model struct {
	table           table.Model
	hlcmd           HledgerCmd
	showBalanceView bool
}

func newModel(hl hledger.Hledger) model {
	return model{
		hlcmd:           NewHledgerCmd(hl),
		showBalanceView: false,
	}
}

func (m model) Init() tea.Cmd {
	return m.hlcmd.runMyCommand("expenses")
}

func (m model) Update(msg tea.Msg) (tea.Model, tea.Cmd) {
	var cmd tea.Cmd
	switch msg := msg.(type) {
	case tea.KeyMsg:
		switch msg.String() {
		case "esc":
			if m.table.Focused() {
				m.table.Blur()
			} else {
				m.table.Focus()
			}
		case "q", "ctrl+c":
			return m, tea.Quit
		case "enter":
			return m, tea.Batch(
				tea.Printf("Let's go to %s!", m.table.SelectedRow()[1]),
			)
		case "/":
			return m, m.hlcmd.runMyCommand("dbs_twisha")
		case "v":
			m.showBalanceView = !m.showBalanceView
		}
	case tableData:
		m.table.SetRows(msg.rows)
		return m, nil
	}
	m.table, cmd = m.table.Update(msg)
	return m, cmd
}

func (m model) View() string {
	if m.showBalanceView {
		return balanceView()
	}

	return baseStyle.Render(m.table.View()) + "\n"
}

func balanceView() string {
	return fmt.Sprintf("Hey!\nWe will show the Balance view here.\nWIP, come back later\n")
}
