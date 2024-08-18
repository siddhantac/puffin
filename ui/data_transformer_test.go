package ui

import (
	"testing"

	"github.com/charmbracelet/bubbles/table"
	"github.com/matryer/is"
)

func TestAccountTreeMode(t *testing.T) {
	is := is.New(t)

	treeViewEnabled := func() bool { return true }
	// treeViewDisabled := func() bool { return false }

	atv := newAccountTreeView(treeViewEnabled)
	rows := []table.Row{
		{"a:b:c", "x", "y"},
		{"assets:bank", "x", "y"},
	}

	err := atv.Transform(rows)
	is.NoErr(err)

	expected := []table.Row{
		{"  c", "x", "y"},
		{" bank", "x", "y"},
	}
	is.Equal(expected, rows)
}
