package ui

import (

	"github.com/charmbracelet/lipgloss"
)

type monthsView struct {
	active        bool
	selectedIndex int // 0..11 (Jan..Dec)
	months        []string
}

func newMonthsView() *monthsView {
	return &monthsView{
		active:        false,
		selectedIndex: 0,
		months: []string{
			"January", "February", "March", "April", "May", "June",
			"July", "August", "September", "October", "November", "December",
		},
	}
}

func (mv *monthsView) IsActive() bool { return mv.active }
func (mv *monthsView) Toggle()        { mv.active = !mv.active }
func (mv *monthsView) Activate()      { mv.active = true }
func (mv *monthsView) Deactivate()    { mv.active = false }
func (mv *monthsView) SelectFirst()   { mv.selectedIndex = 0 }
func (mv *monthsView) Next()          { mv.selectedIndex = (mv.selectedIndex + 1) % 12 }
func (mv *monthsView) Prev()          { mv.selectedIndex = (mv.selectedIndex + 11) % 12 }
func (mv *monthsView) SelectedMonthIndex() int { return mv.selectedIndex }

func (mv *monthsView) View() string {
	title := sectionTitleStyle.Copy().Render("m: toggle months  ←/→: change  esc: close")

	linkStyle := lipgloss.NewStyle().
		Foreground(theme.PrimaryForeground)
	selectedStyle := linkStyle.Copy().
		Background(theme.Accent).
		Foreground(theme.PrimaryColor).
		Bold(true)

	// Split months into two columns: left (Jan–Jun) and right (Jul–Dec)
	leftCol := mv.months[:6]
	rightCol := mv.months[6:]

	// Compute a fixed width for the left column so the right column lines up nicely
	leftMax := 0
	for _, m := range leftCol {
		if w := lipgloss.Width(m); w > leftMax {
			leftMax = w
		}
	}

	rows := []string{title}
	for i := 0; i < 6; i++ {
		lStyle := linkStyle
		rStyle := linkStyle
		if mv.active && mv.selectedIndex == i {
			lStyle = selectedStyle
		}
		if mv.active && mv.selectedIndex == i+6 {
			rStyle = selectedStyle
		}

		leftCell := lStyle.Copy().Width(leftMax).Render(leftCol[i])
		gap := "  "
		row := lipgloss.JoinHorizontal(lipgloss.Top, leftCell, gap, rStyle.Render(rightCol[i]))
		rows = append(rows, row)
	}

	sectionStyle := lipgloss.NewStyle().
		MarginRight(1).
		MarginLeft(1).
		BorderStyle(lipgloss.NormalBorder()).
		BorderForeground(theme.PrimaryForeground).
		BorderBottom(true)

	return sectionStyle.Render(
		lipgloss.JoinVertical(lipgloss.Left, rows...),
	)
}
