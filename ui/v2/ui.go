package ui

import (
	"fmt"
	"io"
	"log"
	"os"

	"github.com/siddhantac/puffin/ui/v2/hledger"

	"github.com/charmbracelet/bubbles/spinner"
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
	// var p *tea.Program
	cmdRunner := newCmdRunner()
	p := tea.NewProgram(newUI(cmdRunner))
	cmdRunner.p = p
	cmdRunner.listen()
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
	tabTitles  []string
	tabContent []tea.Model
	activeTab  int
	cmdRunner  *cmdRunner

	captureKeysMode bool
}

func newUI(cr *cmdRunner) *ui {
	return &ui{
		tabTitles: []string{
			"Home",
			"Reports",
		},
		tabContent: []tea.Model{
			newHome(hledger.HledgerData{}, cr),
			newReports(hledger.HledgerData{}, cr),
		},
		captureKeysMode: true,
		cmdRunner:       cr,
	}
}

func (u *ui) Init() tea.Cmd {
	batchCmds := []tea.Cmd{
		tea.EnterAltScreen,
		u.tabContent[0].Init(),
		u.tabContent[1].Init(),
	}
	return tea.Sequence(batchCmds...)
}

func (u *ui) Update(msg tea.Msg) (tea.Model, tea.Cmd) {
	if _, ok := msg.(spinner.TickMsg); !ok {
		log.Printf("ui: msg: %T | %v", msg, msg)
	}
	var cmd tea.Cmd

	switch msg := msg.(type) {
	case tea.WindowSizeMsg:
		u.updateAll(msg)
		return u, cmd
	case stopCaptureKeysMsg:
		u.captureKeysMode = false

	case cancelFilterMsg, applyFilterMsg:
		u.tabContent[u.activeTab], cmd = u.tabContent[u.activeTab].Update(msg)
		return u, cmd

	case queryBalance, updateBalance, updateRegister, queryRegister, clearRegister:
		u.tabContent[0], cmd = u.tabContent[0].Update(msg)
		return u, cmd

	case queryIncomeStatement, updateIncomeStatement, queryBalanceSheet, updateBalanceSheet, updateReports:
		u.tabContent[1], cmd = u.tabContent[1].Update(msg)
		return u, cmd

	case tea.KeyMsg:
		if u.captureKeysMode {
			switch msg.String() {
			case "/":
				u.tabContent[u.activeTab], cmd = u.tabContent[u.activeTab].Update(activateFilterMsg{})
				return u, tea.Sequence(stopCaptureKeysCmd, cmd)
			case "tab":
				u.activeTab = min(u.activeTab+1, len(u.tabContent)-1)
			case "shift+tab":
				u.activeTab = max(u.activeTab-1, 0)
			case "q":
				return u, tea.Quit
			}
		}
		switch msg.String() {
		case "enter", "esc":
			u.captureKeysMode = true
		}
		cmd = u.updateAll(msg)
		return u, cmd

	default:
		cmd = u.updateAll(msg)
		return u, cmd

	}

	return u, nil
}

func (u *ui) updateAll(msg tea.Msg) tea.Cmd {
	var batchCmds []tea.Cmd
	var cmd tea.Cmd
	for i, c := range u.tabContent {
		u.tabContent[i], cmd = c.Update(msg)
		batchCmds = append(batchCmds, cmd)
	}
	return tea.Batch(batchCmds...)
}

func (u *ui) View() string {
	renderedTabs := make([]string, 0)
	for i, t := range u.tabTitles {
		if i == u.activeTab {
			renderedTabs = append(renderedTabs, activeTabStyle.Render(t))
		} else {
			renderedTabs = append(renderedTabs, inactiveTabStyle.Render(t))
		}
	}
	content := u.tabContent[u.activeTab].View()
	return lipgloss.JoinVertical(
		lipgloss.Left,
		lipgloss.JoinHorizontal(lipgloss.Top, renderedTabs...),
		content,
	)
}
