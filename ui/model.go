package ui

import (
	"puffin/hledger"

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
	searchFilter             hledger.Filter
	isTxnsSortedByMostRecent bool
}

func newModel(hl hledger.Hledger) *model {
	t := &model{
		hlcmd:                    NewHledgerCmd(hl),
		registerTable:            buildTable(registerColumns()),
		balanceTable:             buildTable(balanceColumns()),
		help:                     newHelpModel(),
		activeRegisterDateFilter: hledger.NewDateFilter().UpToToday(),
		activeBalanceDateFilter:  hledger.NewDateFilter().UpToToday(),
		activeAccountFilter:      hledger.NoFilter{},
		isTxnsSortedByMostRecent: true,
	}

	t.registerTable.Focus()
	t.balanceTable.Blur()
	return t
}

func (m *model) Init() tea.Cmd {
	return tea.Batch(
		m.hlcmd.register(m.isTxnsSortedByMostRecent, m.activeRegisterDateFilter),
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
		case key.Matches(msg, m.help.keys.Search):
			form := newFilterForm(m, searchFilter)
			return form.Update(nil)

		case key.Matches(msg, m.help.keys.Refresh): // manual refresh
			return m, m.refresh()
		case key.Matches(msg, m.help.keys.Quit):
			m.quitting = true
			return m, tea.Quit

		case key.Matches(msg, m.help.keys.SwapSortingByDate):
			m.isTxnsSortedByMostRecent = !m.isTxnsSortedByMostRecent
			return m, m.hlcmd.register(m.isTxnsSortedByMostRecent, m.activeAccountFilter, m.activeRegisterDateFilter)
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
		case hledger.DescriptionFilter:
			m.searchFilter = msg
		}

		return m, m.refresh()
	}

	return m, nil
}

func (m *model) search(query string) tea.Cmd {
	return tea.Cmd(
		m.hlcmd.register(m.isTxnsSortedByMostRecent,
			m.activeAccountFilter,
			m.activeRegisterDateFilter,
			m.searchFilter,
		),
	)
}

func (m *model) refresh() tea.Cmd {
	return tea.Batch(
		m.hlcmd.register(m.isTxnsSortedByMostRecent,
			m.activeAccountFilter,
			m.activeRegisterDateFilter,
			m.searchFilter,
		),
		m.hlcmd.balance(m.activeAccountFilter, m.activeBalanceDateFilter),
	)
}

func (m *model) View() string {
	if m.quitting {
		return ""
	}

	var regView, balView string

	if m.registerTable.Focused() {
		regView = activeTableStyle.Render(m.registerTable.View())
		balView = inactiveTableStyle.Render(m.balanceTable.View())
	} else if m.balanceTable.Focused() {
		regView = inactiveTableStyle.Render(m.registerTable.View())
		balView = activeTableStyle.Render(m.balanceTable.View())
	}

	registerView := lipgloss.JoinVertical(lipgloss.Left, titleTextStyle.Render("Register"), regView)
	balanceView := lipgloss.JoinVertical(lipgloss.Left, titleTextStyle.Render("Balance"), balView)
	tablesView := lipgloss.JoinHorizontal(lipgloss.Left, registerView, balanceView)

	return lipgloss.JoinVertical(lipgloss.Left, m.help.View(), tablesView)
}
