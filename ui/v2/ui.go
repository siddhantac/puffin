package ui

import (
	"fmt"
	"io"
	"log"
	"os"

	"github.com/siddhantac/puffin/ui/v2/hledger"

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

	// Track terminal dimensions for footer rendering
	width  int
	height int
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
	var cmd tea.Cmd

	switch msg := msg.(type) {
	case tea.WindowSizeMsg:
		log.Printf("ui: msg: %T", msg)
		u.width = msg.Width
		u.height = msg.Height
		u.updateAll(msg)
		return u, cmd
	case stopCaptureKeysMsg:
		log.Printf("ui: msg: %T", msg)
		u.captureKeysMode = false

	case blurFilterMsg, refreshDataMsg:
		log.Printf("ui: msg: %T", msg)
		u.tabContent[u.activeTab], cmd = u.tabContent[u.activeTab].Update(msg)
		return u, cmd

	case queryBalance, updateBalance, updateRegister, queryRegister, clearRegister:
		log.Printf("ui: msg: %T", msg)
		u.tabContent[0], cmd = u.tabContent[0].Update(msg)
		return u, cmd

	case queryIncomeStatement, updateIncomeStatement, queryBalanceSheet, updateBalanceSheet, updateReports:
		log.Printf("ui: msg: %T", msg)
		u.tabContent[1], cmd = u.tabContent[1].Update(msg)
		return u, cmd

	case tea.KeyMsg:
		log.Printf("ui: msg: %T | %v", msg, msg)
		if u.captureKeysMode {
			switch msg.String() {
			case "/":
				u.tabContent[u.activeTab], cmd = u.tabContent[u.activeTab].Update(focusFilterMsg{})
				return u, tea.Sequence(stopCaptureKeysCmd, cmd)
			case "tab":
				u.activeTab = min(u.activeTab+1, len(u.tabContent)-1)
				return u, nil
			case "shift+tab":
				u.activeTab = max(u.activeTab-1, 0)
				return u, nil
			case "q":
				// Back to top menu: do not quit. Ensure we are in top-level capture mode and blur any focused filters
				u.captureKeysMode = true
				u.tabContent[u.activeTab], cmd = u.tabContent[u.activeTab].Update(blurFilterMsg{})
				return u, cmd
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

// renderColumnMarkers returns a single-line string of length `width`
// with '|' at every 10th column (10,20,30,...), spaces elsewhere.
func renderColumnMarkers(width int) string {
	if width <= 0 {
		return ""
	}
	b := make([]rune, width)
	for i := 0; i < width; i++ {
		if (i+1)%10 == 0 {
			b[i] = '|'
		} else {
			b[i] = ' '
		}
	}
	return string(b)
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
	footer := lipgloss.NewStyle().Width(u.width).Render(renderColumnMarkers(u.width))
	return lipgloss.JoinVertical(
		lipgloss.Left,
		lipgloss.JoinHorizontal(lipgloss.Top, renderedTabs...),
		content,
		footer,
	)
}
