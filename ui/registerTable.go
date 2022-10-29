package ui

import (
	"hledger/hledger"

	"github.com/charmbracelet/bubbles/key"
	"github.com/charmbracelet/bubbles/table"
	tea "github.com/charmbracelet/bubbletea"
	"github.com/charmbracelet/lipgloss"
)

type model struct {
	registerTable table.Model
	balanceTable  table.Model
	help          helpModel
	hlcmd         HledgerCmd
	quitting      bool
	isFormDisplay bool

	activeRegisterDateFilter hledger.Filter
	activeBalanceDateFilter  hledger.Filter
	activeAccountFilter      hledger.Filter
}

func newModel(hl hledger.Hledger) *model {
	t := &model{
		hlcmd:                    NewHledgerCmd(hl),
		registerTable:            buildTable(registerColumns()),
		balanceTable:             buildTable(balanceColumns()),
		help:                     newHelpModel(),
		activeRegisterDateFilter: hledger.NewDateFilter().UpToLastMonth(),
		activeBalanceDateFilter:  hledger.NewDateFilter().UpToLastMonth(),
		activeAccountFilter:      hledger.NoFilter{},
	}

	t.registerTable.Focus()
	t.balanceTable.Blur()
	return t
}

func (m *model) Init() tea.Cmd {
	return tea.Batch(
		m.hlcmd.register(m.activeRegisterDateFilter),
		m.hlcmd.balance(m.activeBalanceDateFilter),
	)
}

func (m *model) Update(msg tea.Msg) (tea.Model, tea.Cmd) {
	switch msg := msg.(type) {

	case tea.KeyMsg:
		switch {
		// navigation
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
		case key.Matches(msg, m.help.keys.Switch):
			if m.registerTable.Focused() {
				m.balanceTable.Focus()
				m.registerTable.Blur()
			} else if m.balanceTable.Focused() {
				m.registerTable.Focus()
				m.balanceTable.Blur()
			}
			return m, nil

			// help
		case key.Matches(msg, m.help.keys.Help):
			m.help.help.ShowAll = !m.help.help.ShowAll

			// filters
		case key.Matches(msg, m.help.keys.AccountFilter):
			form := newFilterForm(m, accountFilter)
			return form.Update(nil)
		case key.Matches(msg, m.help.keys.DateFilter):
			form := newFilterForm(m, dateFilter)
			return form.Update(nil)

		case key.Matches(msg, m.help.keys.Refresh): // manual refresh
			return m, m.refresh()
		case key.Matches(msg, m.help.keys.Quit):
			m.quitting = true
			return m, tea.Quit
		}

	case accountsData: // set table data when it changes
		m.balanceTable.SetRows(msg)
	case transactionsData: // set table data when it changes
		m.registerTable.SetRows(msg)

	case hledger.Filter:
		switch msg := msg.(type) {
		case hledger.AccountFilter:
			m.activeAccountFilter = msg
		case hledger.DateFilter:
			m.activeBalanceDateFilter = msg
			m.activeRegisterDateFilter = msg
		}

		return m, m.refresh()
	}

	return m, nil
}

func (m *model) refresh() tea.Cmd {
	return tea.Batch(
		m.hlcmd.register(m.activeAccountFilter, m.activeRegisterDateFilter),
		m.hlcmd.balance(m.activeAccountFilter, m.activeBalanceDateFilter),
	)
}

var titleTextStyle = lipgloss.NewStyle().
	Bold(true).
	Background(lipgloss.Color("55")).
	PaddingLeft(1).
	PaddingRight(1).
	MarginTop(1)

func (m *model) View() string {
	if m.quitting {
		return ""
	}

	registerView := lipgloss.JoinVertical(lipgloss.Left, titleTextStyle.Render("Register"), tableStyle.Render(m.registerTable.View()))
	balanceView := lipgloss.JoinVertical(lipgloss.Left, titleTextStyle.Render("Balance"), tableStyle.Render(m.balanceTable.View()))
	tablesView := lipgloss.JoinHorizontal(lipgloss.Left, registerView, balanceView)

	return lipgloss.JoinVertical(lipgloss.Left, m.help.View(), tablesView)
}
