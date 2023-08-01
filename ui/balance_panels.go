package ui

import (
	tea "github.com/charmbracelet/bubbletea"
	"github.com/charmbracelet/lipgloss"
)

type balancePanels struct {
	assetsTable      *assetsTable
	revenueTable     *assetsTable
	expensesTable    *assetsTable
	liabilitiesTable *assetsTable
}

func newBalancePanels() *balancePanels {
	return &balancePanels{
		assetsTable: newAssetsTable(),
	}
}

func (b *balancePanels) Init() tea.Cmd {
	return nil
}

func (b *balancePanels) Update(msg tea.Msg) (tea.Model, tea.Cmd) {
	return b.assetsTable.Update(msg)
}

func (b *balancePanels) View() string {
	return lipgloss.JoinVertical(lipgloss.Top,
		lipgloss.JoinHorizontal(lipgloss.Left,
			b.assetsTable.View(),
			b.assetsTable.View()),
		lipgloss.JoinHorizontal(lipgloss.Left,
			b.assetsTable.View(),
			b.assetsTable.View()),
	)
}
