package ui

import (
	"github.com/charmbracelet/bubbles/viewport"
	tea "github.com/charmbracelet/bubbletea"
	"github.com/charmbracelet/lipgloss"
	"github.com/siddhantac/puffin/ui/v2/interfaces"
)

type setViewportContentMsg struct {
	content string
}

type viewportTab struct {
	viewport            viewport.Model
	ready               bool
	content             string
	filterGroup         *filterGroup
	displayOptionsGroup *displayOptionsGroup
	height, width       int
}

func newViewportTab() *viewportTab {
	optionFactory := displayOptionsGroupFactory{}
	filterGroupFactory := filterGroupFactory{}
	return &viewportTab{
		content:             data,
		filterGroup:         filterGroupFactory.NewGroupReports(),
		displayOptionsGroup: optionFactory.NewReportsGroup(interfaces.Yearly, 3, interfaces.ByAccount),
	}
}

func (v *viewportTab) Init() tea.Cmd {
	return nil
}

func (v *viewportTab) Update(msg tea.Msg) (tea.Model, tea.Cmd) {
	var cmd tea.Cmd
	switch msg := msg.(type) {
	case tea.WindowSizeMsg:
		v.width = msg.Width
		v.height = msg.Height

		fg, _ := v.filterGroup.Update(msg)
		v.filterGroup = fg.(*filterGroup)

		if !v.ready {
			v.viewport = viewport.New(msg.Width, msg.Height-6)
			v.viewport.SetContent(v.content)
			v.ready = true
		} else {
			v.viewport.Width = msg.Width
			v.viewport.Height = msg.Height - 6
		}
		return v, nil

	case focusFilterMsg:
		v.filterGroup.Focus()
		return v, nil

	case blurFilterMsg:
		v.filterGroup.Blur()
		return v, nil

	case refreshDataMsg:
		v.filterGroup.Blur()
		return v, nil

	case setViewportContentMsg:
		v.content = msg.content
		if v.ready {
			v.viewport.SetContent(v.content)
		}
		return v, nil

	case tea.KeyMsg:
		if v.filterGroup.Focused() {
			fg, cmd := v.filterGroup.Update(msg)
			v.filterGroup = fg.(*filterGroup)
			return v, cmd
		}

		dg, cmd := v.displayOptionsGroup.Update(msg)
		v.displayOptionsGroup = dg.(*displayOptionsGroup)
		if cmd != nil {
			return v, cmd
		}
	}

	v.viewport, cmd = v.viewport.Update(msg)
	return v, cmd
}

func (v *viewportTab) View() string {
	if !v.ready {
		return "\n  Loading..."
	}

	filterView := lipgloss.JoinHorizontal(
		lipgloss.Center,
		v.filterGroup.View(),
		" ",
		lipgloss.NewStyle().
			Border(lipgloss.RoundedBorder(), false, false, false, true).
			BorderForeground(lipgloss.Color("240")).
			Render(divider.View()),
		" ",
		v.displayOptionsGroup.View(),
	)

	return lipgloss.JoinVertical(
		lipgloss.Left,
		filterView,
		lipgloss.NewStyle().
			PaddingLeft(2).
			Render(v.viewport.View()),
	)
}

var data = `Yearly Income Statement 2023-01-01..2026-12-31

┌──────────────────────────╥─────────────────┬─────────────────┬─────────────────┬────────────────┐
│                          ║            2023 │            2024 │            2025 │           2026 │
╞══════════════════════════╬═════════════════╪═════════════════╪═════════════════╪════════════════╡
│ Revenues                 ║                 │                 │                 │                │
├──────────────────────────╫─────────────────┼─────────────────┼─────────────────┼────────────────┤
│ income:bonus             ║      50.00 SGD$ │   1,932.42 SGD$ │      54.92 SGD$ │              0 │
│ income:cashback          ║     360.12 SGD$ │      92.16 SGD$ │      63.20 SGD$ │              0 │
│ income:interest          ║   2,739.79 SGD$ │   4,022.36 SGD$ │   3,577.70 SGD$ │    208.52 SGD$ │
│ income:investment        ║   1,802.03 SGD$ │   3,690.52 SGD$ │   2,589.08 SGD$ │    958.20 SGD$ │
│ income:lend              ║     282.88 SGD$ │               0 │     386.00 SGD$ │              0 │
│ income:others            ║     258.30 SGD$ │               0 │               0 │              0 │
│ income:royalties         ║               0 │     122.40 SGD$ │               0 │              0 │
│ income:unknown           ║               0 │               0 │     223.45 SGD$ │              0 │
│ income:salary            ║ 242,954.30 SGD$ │ 166,356.80 SGD$ │ 215,863.14 SGD$ │ 22,620.00 SGD$ │
├──────────────────────────╫─────────────────┼─────────────────┼─────────────────┼────────────────┤
│                          ║ 248,447.42 SGD$ │ 176,216.66 SGD$ │ 222,757.49 SGD$ │ 23,786.72 SGD$ │
╞══════════════════════════╬═════════════════╪═════════════════╪═════════════════╪════════════════╡
│ Expenses                 ║                 │                 │                 │                │
├──────────────────────────╫─────────────────┼─────────────────┼─────────────────┼────────────────┤
│ expenses:apparel         ║   1,481.74 SGD$ │   2,279.42 SGD$ │   1,738.15 SGD$ │     38.50 SGD$ │
│ expenses:beauty          ║               0 │               0 │     212.70 SGD$ │     70.42 SGD$ │
│ expenses:charity         ║   2,550.00 SGD$ │     100.00 SGD$ │      50.00 SGD$ │     35.00 SGD$ │
│ expenses:credit_card_fee ║     232.20 SGD$ │     234.35 SGD$ │     109.95 SGD$ │              0 │
│ expenses:entertainment   ║   1,611.43 SGD$ │     565.02 SGD$ │   1,373.26 SGD$ │              0 │
│ expenses:fitness         ║   6,956.99 SGD$ │   1,969.06 SGD$ │   1,995.92 SGD$ │    512.00 SGD$ │
│ expenses:food            ║  18,138.18 SGD$ │  15,144.42 SGD$ │  16,342.95 SGD$ │  2,494.77 SGD$ │
│ expenses:gift            ║   1,290.49 SGD$ │     176.22 SGD$ │     730.29 SGD$ │     55.52 SGD$ │
│ expenses:groceries       ║   3,622.11 SGD$ │   5,482.15 SGD$ │   4,408.03 SGD$ │    571.53 SGD$ │
│ expenses:household       ║  13,224.02 SGD$ │  14,987.96 SGD$ │  10,579.74 SGD$ │    610.08 SGD$ │
│ expenses:insurance       ║   2,613.73 SGD$ │   2,903.56 SGD$ │   2,921.66 SGD$ │     39.34 SGD$ │
│ expenses:learning        ║      95.20 SGD$ │   1,495.39 SGD$ │      13.38 SGD$ │  1,060.82 SGD$ │
│ expenses:leisure         ║     913.49 SGD$ │               0 │     465.00 SGD$ │              0 │
│ expenses:lend            ║               0 │               0 │     386.00 SGD$ │              0 │
│ expenses:medical         ║     997.62 SGD$ │     941.84 SGD$ │   2,794.83 SGD$ │     30.00 SGD$ │
│ expenses:others          ║     111.26 SGD$ │      19.92 SGD$ │       1.53 SGD$ │              0 │
│ expenses:personal        ║   9,536.17 SGD$ │   4,114.76 SGD$ │   5,040.97 SGD$ │  3,961.39 SGD$ │
│ expenses:petty_cash      ║   1,040.00 SGD$ │     560.00 SGD$ │               0 │              0 │
│ expenses:rent            ║  36,000.00 SGD$ │  51,645.00 SGD$ │  50,400.00 SGD$ │  4,200.00 SGD$ │
│ expenses:subscription    ║   2,274.34 SGD$ │   2,471.91 SGD$ │   2,032.73 SGD$ │    151.73 SGD$ │
│ expenses:tax             ║   8,359.75 SGD$ │  11,617.20 SGD$ │  18,850.98 SGD$ │  1,770.52 SGD$ │
│ expenses:transport       ║   4,680.76 SGD$ │   2,644.20 SGD$ │   3,522.36 SGD$ │    781.69 SGD$ │
│ expenses:travel          ║  21,579.63 SGD$ │  12,831.84 SGD$ │  21,815.89 SGD$ │     26.00 SGD$ │
│ expenses:utilities       ║   2,358.29 SGD$ │   3,306.47 SGD$ │   2,540.62 SGD$ │    253.82 SGD$ │
│ expenses:books           ║     238.42 SGD$ │     521.52 SGD$ │     122.04 SGD$ │              0 │
│ expenses:investment      ║               0 │               0 │      50.90 SGD$ │              0 │
├──────────────────────────╫─────────────────┼─────────────────┼─────────────────┼────────────────┤
│                          ║ 139,905.82 SGD$ │ 136,012.21 SGD$ │ 148,499.88 SGD$ │ 16,663.13 SGD$ │
╞══════════════════════════╬═════════════════╪═════════════════╪═════════════════╪════════════════╡
│ Net:                     ║ 108,541.60 SGD$ │  40,204.45 SGD$ │  74,257.61 SGD$ │  7,123.59 SGD$ │
└──────────────────────────╨─────────────────┴─────────────────┴─────────────────┴────────────────┘
`
