package ui

import (
	"log"

	tea "github.com/charmbracelet/bubbletea"
)

type cmdRunner struct {
	workChan chan func() tea.Msg
	p        *tea.Program
}

func newCmdRunner() *cmdRunner {
	cr := &cmdRunner{
		workChan: make(chan func() tea.Msg, 1),
	}

	return cr
}

type command func() tea.Msg

func (cr *cmdRunner) Run(cmd command) {
	go func() {
		cr.workChan <- cmd
	}()
}

func (cr *cmdRunner) listen() {
	go func() {
		log.Println("command runner listening")
		for f := range cr.workChan {
			if cr.p == nil {
				continue
			}

			msg := f()
			cr.p.Send(msg)
		}
	}()
}
