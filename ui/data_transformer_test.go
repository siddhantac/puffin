package ui

import (
	"testing"

	"github.com/charmbracelet/bubbles/table"
	"github.com/matryer/is"
)

func TestAccountTreeMode(t *testing.T) {
	is := is.New(t)

	atm := newAccountTreeMode(true, cmdBalance)
	row := table.Row{"a:b:c", "x", "y"}

	got, err := atm.Transform(row)
	is.NoErr(err)

	expected := "\t\tc"
	is.Equal(expected, got[0])
}
