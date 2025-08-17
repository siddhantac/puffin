package ui

import (
	"strings"

	tea "github.com/charmbracelet/bubbletea"
	"github.com/charmbracelet/lipgloss"
)

type GraphType int

const (
	GraphLinear GraphType = iota
	GraphQuadratic
	GraphCubic
)

type graphView struct {
	selectedGraph int
	showGraph     bool
	currentGraph  GraphType
	graphs        []GraphOption
}

type GraphOption struct {
	name string
	typ  GraphType
}

func newGraphView() *graphView {
	return &graphView{
		selectedGraph: 0,
		showGraph:     false,
		graphs: []GraphOption{
			{"one", GraphLinear},
			{"two", GraphQuadratic}, 
			{"three", GraphCubic},
		},
	}
}

func (g *graphView) Init() tea.Cmd {
	return nil
}

func (g *graphView) Update(msg tea.Msg) (tea.Model, tea.Cmd) {
	switch msg := msg.(type) {
	case tea.KeyMsg:
		if g.showGraph {
			switch msg.String() {
			case "esc", "q":
				g.showGraph = false
				return g, nil
			}
		} else {
			switch msg.String() {
			case "enter":
				g.currentGraph = g.graphs[g.selectedGraph].typ
				g.showGraph = true
				return g, nil
			case "1":
				g.currentGraph = GraphLinear
				g.showGraph = true
				return g, nil
			case "2":
				g.currentGraph = GraphQuadratic
				g.showGraph = true
				return g, nil
			case "3":
				g.currentGraph = GraphCubic
				g.showGraph = true
				return g, nil
			}
		}
	}
	return g, nil
}

func (g *graphView) View() string {
	// Always show the graph selection in the left panel
	graphSectionTitleStyle := sectionTitleStyle.Copy().Render("GRAPHS")
	
	linkStyle := lipgloss.NewStyle().
		Foreground(theme.PrimaryForeground).
		MarginRight(2)
	
	selectedLinkStyle := linkStyle.Copy().
		Background(theme.Accent).
		Foreground(theme.PrimaryColor).
		Bold(true)
	
	var links []string
	links = append(links, graphSectionTitleStyle)
	
	for i, graph := range g.graphs {
		var style lipgloss.Style
		if i == g.selectedGraph {
			style = selectedLinkStyle
		} else {
			style = linkStyle
		}
		links = append(links, style.Render(graph.name))
	}
	
	sectionStyle := lipgloss.NewStyle().
		MarginRight(1).
		MarginLeft(1).
		BorderStyle(lipgloss.NormalBorder()).
		BorderForeground(theme.PrimaryForeground).
		BorderBottom(true)
	
	return sectionStyle.Render(
		lipgloss.JoinVertical(
			lipgloss.Right,
			links...,
		),
	)
}

func (g *graphView) RenderGraph() string {
	width := 60
	height := 30
	minX, maxX := -10.0, 10.0
	minY, maxY := -10.0, 10.0
	
	// Create a 2D grid for the plot
	grid := make([][]rune, height)
	for i := range grid {
		grid[i] = make([]rune, width)
		for j := range grid[i] {
			grid[i][j] = ' '
		}
	}
	
	// Draw axes
	centerX := width / 2
	centerY := height / 2
	
	// Draw Y-axis
	for y := 0; y < height; y++ {
		grid[y][centerX] = '│'
	}
	
	// Draw X-axis
	for x := 0; x < width; x++ {
		grid[centerY][x] = '─'
	}
	
	// Draw origin
	grid[centerY][centerX] = '┼'
	
	// Plot the function
	for x := 0; x < width; x++ {
		// Convert screen x to mathematical x
		mathX := minX + (float64(x)/float64(width-1))*(maxX-minX)
		
		var mathY float64
		switch g.currentGraph {
		case GraphLinear:
			mathY = mathX // y = x
		case GraphQuadratic:
			mathY = mathX * mathX // y = x²
		case GraphCubic:
			mathY = mathX * mathX * mathX // y = x³
		}
		
		// Convert mathematical y to screen y
		if mathY >= minY && mathY <= maxY {
			screenY := int((maxY-mathY)/(maxY-minY)*float64(height-1))
			if screenY >= 0 && screenY < height {
				grid[screenY][x] = '*'
			}
		}
	}
	
	// Convert grid to string
	var lines []string
	for _, row := range grid {
		lines = append(lines, string(row))
	}
	
	var title string
	switch g.currentGraph {
	case GraphLinear:
		title = "Linear Function: y = x"
	case GraphQuadratic:
		title = "Quadratic Function: y = x²"
	case GraphCubic:
		title = "Cubic Function: y = x³"
	}
	
	titleStyle := lipgloss.NewStyle().
		Bold(true).
		Foreground(theme.Accent).
		Align(lipgloss.Center).
		Margin(1)
	
	graphStyle := lipgloss.NewStyle().
		Border(lipgloss.RoundedBorder()).
		BorderForeground(theme.PrimaryForeground).
		Padding(1)
	
	instructions := lipgloss.NewStyle().
		Foreground(theme.PrimaryForeground).
		Align(lipgloss.Center).
		Render("Use arrow keys to navigate, 'q' or 'esc' to return")
	
	return lipgloss.JoinVertical(
		lipgloss.Center,
		titleStyle.Render(title),
		graphStyle.Render(strings.Join(lines, "\n")),
		instructions,
	)
}

func (g *graphView) IsShowingGraph() bool {
	return g.showGraph
}
