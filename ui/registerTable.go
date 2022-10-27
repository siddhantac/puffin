package ui

import (
	"hledger/hledger"

	"github.com/charmbracelet/bubbles/key"
	"github.com/charmbracelet/bubbles/table"
	tea "github.com/charmbracelet/bubbletea"
	"github.com/charmbracelet/lipgloss"
)

type registerTable struct {
	registerTable table.Model
	balanceTable  table.Model
	hlcmd         HledgerCmd
	quitting      bool
	help          helpModel
}

func newRegisterTableModel(hl hledger.Hledger) *registerTable {
	return &registerTable{
		hlcmd:         NewHledgerCmd(hl),
		registerTable: buildTable(registerColumns()),
		balanceTable:  buildTable(balanceColumns()),
		help:          newHelpModel(),
	}
}

func (m *registerTable) Init() tea.Cmd {
	return tea.Batch(
		m.hlcmd.register(hledger.NoFilter{}),
		m.hlcmd.balance(hledger.NoFilter{}),
	)
}

func (m *registerTable) Update(msg tea.Msg) (tea.Model, tea.Cmd) {
	switch msg := msg.(type) {
	case tea.KeyMsg:
		switch {
		case key.Matches(msg, m.help.keys.Up):
			m.registerTable.MoveUp(1)
			return m, nil
		case key.Matches(msg, m.help.keys.Down):
			m.registerTable.MoveDown(1)
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

	case accountsData: // set table data when it changes
		m.balanceTable.SetRows(msg)
	case transactionsData: // set table data when it changes
		m.registerTable.SetRows(msg)

	case hledger.Filter:
		return m, m.hlcmd.register(msg)
	}

	return m, nil
}

var titleTextStyle = lipgloss.NewStyle().
	Bold(true).
	Background(lipgloss.Color("55")).
	PaddingLeft(1).
	PaddingRight(1).
	MarginTop(1)

func (m *registerTable) View() string {
	if m.quitting {
		return ""
	}

	registerView := lipgloss.JoinVertical(lipgloss.Left, titleTextStyle.Render("Register"), tableStyle.Render(m.registerTable.View()))
	balanceView := lipgloss.JoinVertical(lipgloss.Left, titleTextStyle.Render("Balance"), tableStyle.Render(m.balanceTable.View()))
	tablesView := lipgloss.JoinHorizontal(lipgloss.Left, registerView, balanceView)

	return lipgloss.JoinVertical(lipgloss.Left, tablesView, m.help.View())
}
