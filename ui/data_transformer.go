package ui

import (
	"fmt"
	"strings"

	"github.com/charmbracelet/bubbles/table"
	"github.com/charmbracelet/lipgloss"
)

// TODO: move this later to where it is consumed
type dataTransformer interface {
	Transform([]table.Row) error
}

type accountTreeView struct {
	treeView func() bool
}

func newAccountTreeView(treeView func() bool) accountTreeView {
	return accountTreeView{
		treeView: treeView,
	}
}

func (t accountTreeView) Transform(rows []table.Row) error {
	if !t.treeView() {
		return nil // no need to modify data
	}

	for _, row := range rows {
		account := row[0]
		accounts := strings.Split(account, ":")
		row[0] = fmt.Sprintf("%s%s", strings.Repeat(" ", len(accounts)-1), accounts[len(accounts)-1])
	}
	return nil
}

// highlightBSHeaders colors the first cell of rows that are the section headers
// "Assets" or "Liabilities" using the theme's secondary color (Dracula purple in
// the Dracula theme), without altering other cells or row behavior.
// This gives a visual cue similar to the newer UI while keeping legacy layout.
type highlightBSHeaders struct{}

func (h highlightBSHeaders) Transform(rows []table.Row) error {
	if len(rows) == 0 {
		return nil
	}
	style := lipgloss.NewStyle().Foreground(theme.SecondaryColor).Bold(true)
	for i := range rows {
		if len(rows[i]) == 0 { continue }
		name := strings.ToLower(strings.TrimSpace(rows[i][0]))
		if name == "assets" || name == "liabilities" {
			rows[i][0] = style.Render(rows[i][0])
		}
	}
	return nil
}
