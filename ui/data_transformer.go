package ui

import (
	"fmt"
	"strings"

	"github.com/charmbracelet/bubbles/table"
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
