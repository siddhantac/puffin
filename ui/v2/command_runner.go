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
	// msg := cmd()
	// log.Printf(">> writing %T | %v", msg, msg)
	go func() {
		cr.workChan <- cmd
	}()
}

func (cr *cmdRunner) listen() {
	go func() {
		log.Printf(">> listening")
		for f := range cr.workChan {
			if cr.p == nil {
				log.Printf(">> program is nil")
				continue
			}

			msg := f()
			log.Printf(">> sending %T | %v", msg, msg)
			cr.p.Send(msg)
		}
	}()
}
