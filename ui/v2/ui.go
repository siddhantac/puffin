package ui

import (
	"fmt"
	"io"
	"log"
	"os"
	"puffin/ui/v2/cache"
	"puffin/ui/v2/hledger"

	tea "github.com/charmbracelet/bubbletea"
	"github.com/charmbracelet/lipgloss"
)

func Start(isDebug bool) {
	if isDebug {
		f, err := tea.LogToFile("puffin.log", "debug")
		if err != nil {
			panic(err)
		}
		defer f.Close()
	} else {
		log.SetOutput(io.Discard)
	}

	// log.Printf("init puffin %s", Version)
	p := tea.NewProgram(newUI())
	if _, err := p.Run(); err != nil {
		fmt.Println("Error running program:", err)
		os.Exit(1)
	}
}

type ui struct {
	tabs *tabList
}

func newUI() *ui {
	tabList := []*tab{
		&tab{name: "Home", model: newHome(cache.NewCache(hledger.HledgerData{}))},
		&tab{name: "Advanced reports", model: newAdvancedReports(cache.NewCache(hledger.HledgerData{}))},
		// {name: "Home", model: newHome(cache.NewCache(hledger.HledgerData{}))},
		// {name: "Details", model: newDetailView()},
		// {name: "IS", model: Table{}},
		// {name: "BS", model: Table{}},
	}
	return &ui{
		tabs: NewTabList(tabList),
	}
}

func (u *ui) Init() tea.Cmd {
	batchCmds := []tea.Cmd{
		tea.EnterAltScreen,
	}
	for _, t := range u.tabs.tabs {
		batchCmds = append(batchCmds, t.model.Init())
	}
	return tea.Sequence(batchCmds...)
}

func (u *ui) Update(msg tea.Msg) (tea.Model, tea.Cmd) {
	log.Printf("ui: msg: %T | %v", msg, msg)
	var cmd tea.Cmd
	var batchCmds tea.BatchMsg

	// u.tabs, cmd = u.tabs.Update(msg)
	// batchCmds = append(batchCmds, cmd)

	// u.home, cmd = u.home.Update(msg)
	// batchCmds = append(batchCmds, cmd)

	switch msg := msg.(type) {
	case tea.WindowSizeMsg:
		for _, t := range u.tabs.tabs {
			t.model, cmd = t.model.Update(msg)
			batchCmds = append(batchCmds, cmd)
		}
	case tea.KeyMsg:
		switch msg.String() {
		case "q":
			return u, tea.Quit
		case "]":
			u.tabs.NextTab()
			return u, nil
		case "[":
			u.tabs.PrevTab()
			return u, nil
		}
	}
	u.tabs, cmd = u.tabs.Update(msg)
	batchCmds = append(batchCmds, cmd)

	return u, tea.Batch(
		batchCmds...,
	)

}

func (u *ui) View() string {
	view := lipgloss.NewStyle().
		BorderStyle(lipgloss.NormalBorder()).
		BorderTop(true).
		BorderForeground(lipgloss.Color("240")).
		Render(u.tabs.CurrentTab().model.View())

	content := lipgloss.JoinVertical(
		lipgloss.Left,
		u.tabs.View(),
		view,
	)
	return content
}
