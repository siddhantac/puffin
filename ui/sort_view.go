package ui

import (
	"github.com/charmbracelet/lipgloss"
)

func sortingView(isSortByAmount bool) string {
	sortTitleStyle := sectionTitleStyle.Copy().
		MarginTop(1).
		Render("SORT")

	inactiveTextStyle := lipgloss.NewStyle().
		Foreground(theme.PrimaryForeground).
		MarginRight(2)
	activeTextStyle := lipgloss.NewStyle().
		MarginRight(2)

	var (
		sortAccount = inactiveTextStyle.Render("account")
		sortAmount  = inactiveTextStyle.Render("amount")
	)
	if isSortByAmount {
		sortAmount = activeTextStyle.Render("amount")
	} else {
		sortAccount = activeTextStyle.Render("account")
	}

	return lipgloss.JoinVertical(
		lipgloss.Right,
		sortTitleStyle,
		sortAmount,
		sortAccount,
	)
}
