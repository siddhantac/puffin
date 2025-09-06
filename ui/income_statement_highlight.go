package ui

import (
	"strings"

	"github.com/charmbracelet/bubbles/table"
	"github.com/charmbracelet/lipgloss"
)

// highlightISHeaders colors the "Revenues" and "Expenses" lines in the legacy
// Income Statement table to improve visual separation, similar to the newer UI.
// It styles just the first cell in those rows.
//
// We intentionally leave the rest of the layout unchanged so the left panel
// (and other surrounding UI) remains consistent with other reports.

type highlightISHeaders struct{}

func (h highlightISHeaders) Transform(rows []table.Row) error {
	if len(rows) == 0 { return nil }
	style := lipgloss.NewStyle().
		Bold(true).
		Foreground(theme.PrimaryForeground).
		Background(theme.Accent)
	for i := range rows {
		if len(rows[i]) == 0 { continue }
		name := strings.ToLower(strings.TrimSpace(rows[i][0]))
		if name == "revenues" || name == "revenue" || name == "expenses" {
			rows[i][0] = style.Render(rows[i][0])
		}
	}
	return nil
}

