package ui

import tea "github.com/charmbracelet/bubbletea"

type balancePanels struct {
	assetsTable      *TableWrapper
	revenueTable     *TableWrapper
	expensesTable    *TableWrapper
	liabilitiesTable *TableWrapper
}

func newBalancePanels() *balancePanels {
	return &balancePanels{
		assetsTable: NewTableWrapper(newAssetsTable()),
	}
}

func (b *balancePanels) Init() tea.Cmd {
	return nil
}

func (b *balancePanels) Update(msg tea.Cmd) (tea.Model, tea.Cmd) {
	return nil, nil
}

func (b *balancePanels) View() string {
	return ""
}
