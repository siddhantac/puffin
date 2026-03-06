package ui

import (
	"fmt"
	"io"
	"log"
	"os"
	"sync"

	"github.com/siddhantac/puffin/ui/v2/hledger"

	tea "github.com/charmbracelet/bubbletea"
	"github.com/charmbracelet/lipgloss"
)

var once, once1 sync.Once

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
			"1.  Home",
			"2.  Balances",
			"3. 󰠟 Reports",
		},
		tabContent: []tea.Model{
			newHome(hledger.HledgerData{}, cr),
			newBalanceReports(hledger.HledgerData{}, cr),
			newReportsTab(hledger.HledgerData{}, cr),
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
		u.tabContent[2].Init(),
	}
	return tea.Sequence(batchCmds...)
}

func (u *ui) Update(msg tea.Msg) (tea.Model, tea.Cmd) {
	var cmd tea.Cmd

	switch msg := msg.(type) {
	case tea.WindowSizeMsg:
		log.Printf("ui: msg: %T", msg)
		u.updateAll(msg)
		return u, cmd
	case stopCaptureKeysMsg:
		log.Printf("ui: msg: %T", msg)
		u.captureKeysMode = false

	case blurFilterMsg, refreshDataMsg:
		log.Printf("ui: msg: %T", msg)
		u.tabContent[u.activeTab], cmd = u.tabContent[u.activeTab].Update(msg)
		return u, cmd

	// case queryBalance, updateBalance, updateRegister, queryRegister, clearRegister,
	// 	queryBalanceMsg, updateBalanceMsg,
	// 	queryReportsMsg, updateReportsMsg:
	// 	log.Printf("ui: msg: %T", msg)
	// 	batchCmds := []tea.Cmd{}
	// 	for idx, t := range u.tabContent {
	// 		u.tabContent[idx], cmd = t.Update(msg)
	// 		batchCmds = append(batchCmds, cmd)
	// 	}
	// 	// u.tabContent[0], cmd = u.tabContent[0].Update(msg)
	// 	return u, tea.Batch(batchCmds...)

	// case queryBalanceMsg, updateBalanceMsg:
	// 	log.Printf("ui: msg: %T", msg)
	// 	u.tabContent[1], cmd = u.tabContent[1].Update(msg)
	// 	return u, cmd
	//
	// case queryReportsMsg, updateReportsMsg:
	// 	log.Printf("ui: msg: %T", msg)
	// 	u.tabContent[2], cmd = u.tabContent[2].Update(msg)
	// 	return u, cmd

	case tea.KeyMsg:
		log.Printf("ui: msg: %T | %v", msg, msg)
		if u.captureKeysMode {
			switch msg.String() {
			case "/":
				u.tabContent[u.activeTab], cmd = u.tabContent[u.activeTab].Update(focusFilterMsg{})
				return u, tea.Sequence(stopCaptureKeysCmd, cmd)
			case "1":
				u.activeTab = 0
				return u, nil
			case "2":
				once.Do(func() {
					u.tabContent[1], cmd = u.tabContent[1].Update(refreshDataCmd())
				})

				u.activeTab = 1
				return u, cmd
			case "3":
				once1.Do(func() {
					u.tabContent[2], cmd = u.tabContent[2].Update(refreshDataCmd())
				})

				u.activeTab = 2
				return u, cmd
			case "q":
				return u, tea.Quit
			}
		}
		switch msg.String() {
		case "enter", "esc":
			u.captureKeysMode = true
		}
		u.tabContent[u.activeTab], cmd = u.tabContent[u.activeTab].Update(msg)
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
