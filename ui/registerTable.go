package ui

import (
	"hledger/hledger"
	"log"

	"github.com/charmbracelet/bubbles/key"
	"github.com/charmbracelet/bubbles/table"
	tea "github.com/charmbracelet/bubbletea"
	"github.com/charmbracelet/lipgloss"
)

type model struct {
	registerTable table.Model
	balanceTable  table.Model
	hlcmd         HledgerCmd
	quitting      bool
	help          helpModel
}

func newModel(hl hledger.Hledger) *model {
	t := &model{
		hlcmd:         NewHledgerCmd(hl),
		registerTable: buildTable(registerColumns()),
		balanceTable:  buildTable(balanceColumns()),
		help:          newHelpModel(),
	}

	t.registerTable.Focus()
	t.balanceTable.Blur()
	return t
}

func (m *model) Init() tea.Cmd {
	return tea.Batch(
		m.hlcmd.register(hledger.NoFilter{}),
		m.hlcmd.balance(hledger.NoFilter{}),
	)
}

func (m *model) Update(msg tea.Msg) (tea.Model, tea.Cmd) {
	switch msg := msg.(type) {
	case tea.KeyMsg:
		switch {
		case key.Matches(msg, m.help.keys.Up):
			if m.registerTable.Focused() {
				m.registerTable.MoveUp(1)
			} else if m.balanceTable.Focused() {
				m.balanceTable.MoveUp(1)
			}
			return m, nil
		case key.Matches(msg, m.help.keys.Down):
			if m.registerTable.Focused() {
				m.registerTable.MoveDown(1)
			} else if m.balanceTable.Focused() {
				m.balanceTable.MoveDown(1)
			}
			return m, nil
		case key.Matches(msg, m.help.keys.Help):
			m.help.help.ShowAll = !m.help.help.ShowAll
		case key.Matches(msg, m.help.keys.Filter):
			models[registerTableModel] = m // save current state
			form := newFilterForm(m)
			return form.Update(nil)
		case key.Matches(msg, m.help.keys.Switch):
			if m.registerTable.Focused() {
				m.balanceTable.Focus()
				m.registerTable.Blur()
				log.Println("bal focused, reg blurred")
			} else if m.balanceTable.Focused() {
				m.registerTable.Focus()
				m.balanceTable.Blur()
				log.Println("reg focused, bal blurred")
			}
			return m, nil
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
		return m, tea.Batch(
			m.hlcmd.register(msg),
			m.hlcmd.balance(msg),
		)
	}

	return m, nil
}

var titleTextStyle = lipgloss.NewStyle().
	Bold(true).
	Background(lipgloss.Color("55")).
	PaddingLeft(1).
	PaddingRight(1).
	MarginTop(2)

func (m *model) View() string {
	if m.quitting {
		return ""
	}

	registerView := lipgloss.JoinVertical(lipgloss.Left, titleTextStyle.Render("Register"), tableStyle.Render(m.registerTable.View()))
	balanceView := lipgloss.JoinVertical(lipgloss.Left, titleTextStyle.Render("Balance"), tableStyle.Render(m.balanceTable.View()))
	tablesView := lipgloss.JoinHorizontal(lipgloss.Left, registerView, balanceView)

	return lipgloss.JoinVertical(lipgloss.Left, tablesView, m.help.View())
}
