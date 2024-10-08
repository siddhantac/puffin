package ui

import (
	tea "github.com/charmbracelet/bubbletea"
	"github.com/siddhantac/hledger"
)

type content struct {
	msg string
	id  int
	err error
}

// ContentModel extends the tea.Model interface with
// methods which make it possible to dynamically update
// the content and set the model to an "unready" status.
type ContentModel interface {
	tea.Model
	Locked() bool
	IsReady() bool
	SetUnready()
	SetContent(content)
	Run(hledger.Options) tea.Cmd
	Type() cmdType
}

type modelLoading struct{}

func setModelLoading() tea.Msg {
	return modelLoading{}
}

type cmdType string

const (
	cmdBalance  cmdType = "balance"
	cmdRegister cmdType = "register"
	cmdAccounts cmdType = "accounts"
	cmdUnknown  cmdType = "unknown"
)
