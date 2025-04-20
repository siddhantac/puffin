package ui

import (
	"fmt"
	"io"
	"log"
	"os"
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

// type captureKeysMsg struct{}

// func captureKeysCmd() tea.Msg {
// 	return captureKeysMsg{}
// }

type stopCaptureKeysMsg struct{}

func stopCaptureKeysCmd() tea.Msg {
	return stopCaptureKeysMsg{}
}

type ui struct {
	tabs            *tabList
	captureKeysMode bool
}

func newUI() *ui {
	tabList := []*tab{
		{name: "Home", model: newHome(hledger.HledgerData{})},
		{name: "Advanced reports", model: newAdvancedReports(hledger.HledgerData{})},
	}
	return &ui{
		tabs:            NewTabList(tabList),
		captureKeysMode: true,
	}
}

func (u *ui) Init() tea.Cmd {
	batchCmds := []tea.Cmd{
		tea.EnterAltScreen,
		u.tabs.Init(),
	}
	// for _, t := range u.tabs.tabs {
	// 	batchCmds = append(batchCmds, t.model.Init())
	// }
	return tea.Sequence(batchCmds...)
}

func (u *ui) Update(msg tea.Msg) (tea.Model, tea.Cmd) {
	log.Printf("ui: msg: %T | %v", msg, msg)
	var cmd tea.Cmd
	// var batchCmds tea.BatchMsg

	switch msg := msg.(type) {
	case tea.WindowSizeMsg:
		u.tabs, cmd = u.tabs.UpdateAll(msg)
		return u, cmd
		// for _, t := range u.tabs.tabs {
		// 	t.model, cmd = t.model.Update(msg)
		// 	batchCmds = append(batchCmds, cmd)
		// }
	// case captureKeysMsg:
	// 	u.captureKeysMode = true
	case stopCaptureKeysMsg:
		u.captureKeysMode = false

	case tea.KeyMsg:
		if u.captureKeysMode {
			switch msg.String() {
			case "/":
				u.tabs, cmd = u.tabs.Update(activateFilterMsg{})
				return u, tea.Sequence(cmd, stopCaptureKeysCmd)
			case "tab":
				u.tabs.NextTab()
			case "shift+tab":
				u.tabs.PrevTab()
			case "q":
				return u, tea.Quit
			}
		}
		switch msg.String() {
		case "enter", "esc":
			u.captureKeysMode = true
		}
		u.tabs, cmd = u.tabs.Update(msg)
		return u, cmd
	}

	u.tabs, cmd = u.tabs.Update(msg)
	return u, cmd
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
