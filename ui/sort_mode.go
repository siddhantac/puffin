package ui

import (
	"github.com/charmbracelet/lipgloss"
)

func sortingView(isSortByAmount bool) string {
	sortTitleStyle := sectionTitleStyle.Copy()
	sectionTitle := sortTitleStyle.
		MarginTop(1).
		Render("SORT")

	inactiveTextStyle := lipgloss.NewStyle().
		Foreground(theme.PrimaryForeground).
		MarginRight(2)
	textStyle := lipgloss.NewStyle().
		MarginRight(2)

	var sortAmount, sortAccount string
	if isSortByAmount {
		sortAmount = textStyle.Render("amount")
		sortAccount = inactiveTextStyle.Render("account")
	} else {
		sortAmount = inactiveTextStyle.Render("amount")
		sortAccount = textStyle.Render("account")
	}

	return lipgloss.JoinVertical(
		lipgloss.Right,
		sectionTitle,
		sortAmount,
		sortAccount,
	)
}
