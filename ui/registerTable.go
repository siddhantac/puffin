package ui

import (
	"hledger/hledger"

	"github.com/charmbracelet/bubbles/key"
	"github.com/charmbracelet/bubbles/table"
	tea "github.com/charmbracelet/bubbletea"
	"github.com/charmbracelet/lipgloss"
)

type registerTable struct {
	table    table.Model
	hlcmd    HledgerCmd
	quitting bool
	help     helpModel
}

func newRegisterTableModel(hl hledger.Hledger) *registerTable {
	return &registerTable{
		hlcmd: NewHledgerCmd(hl),
		table: buildTable(registerColumns()),
		help:  newHelpModel(),
	}
}

func (m *registerTable) Init() tea.Cmd {
	return m.hlcmd.register(hledger.NoFilter{})
}

func (m *registerTable) Update(msg tea.Msg) (tea.Model, tea.Cmd) {
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
			models[registerTableModel] = m // save current state
			models[filterFormModel] = newFilterForm(registerTableModel)
			return models[filterFormModel].Update(nil)
		case key.Matches(msg, m.help.keys.Switch):
			return models[balanceTableModel], nil
		case key.Matches(msg, m.help.keys.Refresh):
			return m, m.hlcmd.register(hledger.NoFilter{})
		case key.Matches(msg, m.help.keys.Quit):
			m.quitting = true
			return m, tea.Quit
		}
	case tableData: // set table data when it changes
		m.table.SetRows(msg.rows)

	case hledger.Filter:
		return m, m.hlcmd.register(msg)
	}

	return m, nil
}

func (m *registerTable) View() string {
	if m.quitting {
		return ""
	}

	return lipgloss.JoinVertical(lipgloss.Left, baseStyle.Render(m.table.View()), m.help.View())
}
