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
	treeMode bool
	dataType cmdType
}

func newAccountTreeMode(treeMode bool, dataType cmdType) accountTreeMode {
	return accountTreeMode{
		treeMode: treeMode,
		dataType: dataType,
	}
}

func (t accountTreeMode) Transform(data table.Row) (table.Row, error) {
	if t.dataType != cmdBalance {
		return data, nil // do nothing
	}

	if !t.treeMode {
		return data, nil // no need to modify data
	}

	account := data[0]

	accounts := strings.Split(account, ":")
	data[0] = fmt.Sprintf("%s%s", strings.Repeat("\t", len(accounts)-1), accounts[len(accounts)-1])

	return data, nil
}
