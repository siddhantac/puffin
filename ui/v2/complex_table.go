package ui

import (
	"github.com/charmbracelet/bubbles/table"
	tea "github.com/charmbracelet/bubbletea"
	"github.com/charmbracelet/lipgloss"
)

var (
	activeTitleStyle   = lipgloss.NewStyle().Bold(true).Background(lipgloss.Color("57")).PaddingLeft(1).PaddingRight(1)
	inactiveTitleStyle = lipgloss.NewStyle().Bold(true).PaddingLeft(1).PaddingRight(1)
)

func tblStyleActive() (table.Styles, lipgloss.Style) {
	s := table.DefaultStyles()
	s.Header = s.Header.
		BorderStyle(lipgloss.NormalBorder()).
		BorderForeground(lipgloss.Color("240")).
		BorderBottom(true).
		Bold(false)
	s.Selected = s.Selected.
		Foreground(lipgloss.Color("229")).
		Background(lipgloss.Color("57")).
		Bold(false)
	return s, lipgloss.NewStyle().Border(lipgloss.RoundedBorder()).BorderForeground(lipgloss.Color("White"))
}

func tblStyleInactive() (table.Styles, lipgloss.Style) {
	s := table.DefaultStyles()
	s.Header = s.Header.
		BorderStyle(lipgloss.NormalBorder()).
		BorderForeground(lipgloss.Color("240")).
		BorderBottom(true).
		Bold(false)
	s.Selected = s.Selected.
		Foreground(lipgloss.Color("229")).
		Background(lipgloss.Color("60")).
		Bold(false)
	return s, lipgloss.NewStyle().Border(lipgloss.RoundedBorder()).BorderForeground(lipgloss.Color("240"))
}

type complexTable struct {
	title, upperTitle, lowerTitle string
	bottomBar                     table.Model
	upper, lower                  table.Model
	focus                         bool
}

func newComplexTable() *complexTable {
	return &complexTable{
		upper:     table.New(),
		lower:     table.New(),
		bottomBar: table.New(),
	}
}

func (c *complexTable) Focus() {
	c.focus = true
}

func (c *complexTable) Blur() {
	c.focus = false
}

func (c *complexTable) Focused() bool {
	return c.focus
}

func (c *complexTable) Init() tea.Cmd {
	c.upper.Focus()
	c.lower.Blur()
	return nil
}

func (c *complexTable) Update(msg tea.Msg) (*complexTable, tea.Cmd) {
	if !c.focus {
		return c, nil
	}

	var cmd tea.Cmd

	switch msg := msg.(type) {
	case tea.KeyMsg:
		switch msg.String() {
		case ";":
			if c.upper.Focused() {
				c.upper.Blur()
				c.lower.Focus()
			} else {
				c.upper.Focus()
				c.lower.Blur()
			}
		default:
			if c.upper.Focused() {
				c.upper, cmd = c.upper.Update(msg)
			} else {
				c.lower, cmd = c.lower.Update(msg)
			}
		}
	}
	return c, cmd
}

func (c *complexTable) View() string {
	tableStyleActive, styleActive := tblStyleActive()
	tableStyleInactive, styleInactive := tblStyleInactive()

	nonInteractiveTableStyle := table.DefaultStyles()
	nonInteractiveTableStyle.Header = nonInteractiveTableStyle.Header.
		BorderStyle(lipgloss.NormalBorder()).
		BorderForeground(lipgloss.Color("240")).
		BorderBottom(true).
		Bold(false)
	c.bottomBar.SetStyles(nonInteractiveTableStyle)

	var upper, lower string

	if c.upper.Focused() {
		c.upper.SetStyles(tableStyleActive)
		upper = styleActive.Render(c.upper.View())

		c.lower.SetStyles(tableStyleInactive)
		lower = styleInactive.Render(c.lower.View())
	} else {
		c.upper.SetStyles(tableStyleInactive)
		upper = styleInactive.Render(c.upper.View())

		c.lower.SetStyles(tableStyleActive)
		lower = styleActive.Render(c.lower.View())
	}
	return lipgloss.JoinVertical(
		lipgloss.Left,
		lipgloss.JoinVertical(
			lipgloss.Center,
			lipgloss.NewStyle().Bold(true).Render(c.title),
			upper,
		),
		lower,
		styleInactive.Render(c.bottomBar.View()),
	)
}
