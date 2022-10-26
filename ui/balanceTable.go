package ui

import (
	"hledger/hledger"

	"github.com/charmbracelet/bubbles/key"
	"github.com/charmbracelet/bubbles/table"
	tea "github.com/charmbracelet/bubbletea"
	"github.com/charmbracelet/lipgloss"
)

type balanceTable struct {
	table    table.Model
	hlcmd    HledgerCmd
	quitting bool
	help     helpModel
}

func newBalanceTableModel(hl hledger.Hledger) *balanceTable {
	return &balanceTable{
		hlcmd: NewHledgerCmd(hl),
		table: buildTable(balanceColumns()),
		help:  newHelpModel(),
	}
}

func (m *balanceTable) Init() tea.Cmd {
	return m.hlcmd.balance(hledger.NoFilter{})
}

func (m *balanceTable) Update(msg tea.Msg) (tea.Model, tea.Cmd) {
	switch msg := msg.(type) {
	case tea.KeyMsg:
		switch {
		case key.Matches(msg, m.help.keys.Up):
			m.table.MoveUp(1)
			return m, nil
		case key.Matches(msg, m.help.keys.Down):
			m.table.MoveDown(1)
			return m, nil
		case key.Matches(msg, m.help.keys.Help):
			m.help.help.ShowAll = !m.help.help.ShowAll
		case key.Matches(msg, m.help.keys.Filter):
			models[balanceTableModel] = m // save current state
			models[filterFormModel] = newFilterForm(balanceTableModel)
			return models[filterFormModel].Update(nil)
		case key.Matches(msg, m.help.keys.Switch):
			return models[registerTableModel], nil
		case key.Matches(msg, m.help.keys.Refresh):
			return m, m.hlcmd.balance(hledger.NoFilter{})
		case key.Matches(msg, m.help.keys.Quit):
			m.quitting = true
			return m, tea.Quit
		}
	case tableData:
		m.table.SetRows(msg.rows)
	case hledger.Filter:
		return m, m.hlcmd.balance(msg)
	}

	return m, nil
}

func (m *balanceTable) View() string {
	if m.quitting {
		return ""
	}

	return lipgloss.JoinVertical(lipgloss.Left, baseStyle.Render(m.table.View()), m.help.View())
}
