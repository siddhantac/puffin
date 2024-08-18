package ui

import (
	"fmt"
	"strings"

	"github.com/charmbracelet/bubbles/table"
)

// TODO: move this later to where it is consumed
type dataTransformer interface {
	Transform(data table.Row) (table.Row, error)
}

type accountTreeMode struct {
	treeView func() bool
}

func newAccountTreeMode(treeView func() bool) accountTreeMode {
	return accountTreeMode{
		treeView: treeView,
	}
}

func (t accountTreeMode) Transform(data table.Row) (table.Row, error) {
	if !t.treeView() {
		return data, nil // no need to modify data
	}

	account := data[0]

	accounts := strings.Split(account, ":")
	data[0] = fmt.Sprintf("%s%s", strings.Repeat(" ", len(accounts)-1), accounts[len(accounts)-1])

	return data, nil
}
