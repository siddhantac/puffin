package ui

import (
	"bytes"
	"encoding/base64"
	"fmt"
	"math"
	"os"
	"os/exec"
	"sort"
	"strconv"
	"strings"
	"time"

	tea "github.com/charmbracelet/bubbletea"
	"github.com/charmbracelet/lipgloss"
	"github.com/siddhantac/hledger"
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

	// revenue graph data cache
	revData *revenueData
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
			{"revenue", GraphLinear},
			{"B/S", GraphQuadratic},
			{"liabilities", GraphCubic},
		},
		revData: nil,
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
			case "up":
				if len(g.graphs) > 0 {
					g.selectedGraph = (g.selectedGraph - 1 + len(g.graphs)) % len(g.graphs)
					g.currentGraph = g.graphs[g.selectedGraph].typ
				}
				return g, nil
			case "down":
				if len(g.graphs) > 0 {
					g.selectedGraph = (g.selectedGraph + 1) % len(g.graphs)
					g.currentGraph = g.graphs[g.selectedGraph].typ
				}
				return g, nil
			case "1":
				if len(g.graphs) >= 1 {
					g.selectedGraph = 0
					g.currentGraph = g.graphs[g.selectedGraph].typ
				}
				return g, nil
			case "2":
				if len(g.graphs) >= 2 {
					g.selectedGraph = 1
					g.currentGraph = g.graphs[g.selectedGraph].typ
				}
				return g, nil
			case "3":
				if len(g.graphs) >= 3 {
					g.selectedGraph = 2
					g.currentGraph = g.graphs[g.selectedGraph].typ
				}
				return g, nil
			}
		} else {
			switch msg.String() {
			case "enter":
				g.currentGraph = g.graphs[g.selectedGraph].typ
				g.showGraph = true
				return g, nil
			case "1":
				g.selectedGraph = 0
				g.currentGraph = GraphLinear
				g.showGraph = true
				return g, nil
			case "2":
				g.selectedGraph = 1
				g.currentGraph = GraphQuadratic
				g.showGraph = true
				return g, nil
			case "3":
				g.selectedGraph = 2
				g.currentGraph = GraphCubic
				g.showGraph = true
				return g, nil
			}
		}
	}
	return g, nil
}

func (g *graphView) View() string {
	// Plain, fixed-alignment block without borders
	lines := []string{
		" ",
		" G/g to show graphs",
		"           " + sectionTitleStyle.Render("GRAPHS"),
	}
	for _, graph := range g.graphs {
		lines = append(lines, "          "+graph.name)
	}
	lines = append(lines, "──────────────────")
	return strings.Join(lines, "\n")
}

func (g *graphView) RenderGraph() string {
	// Graph one: stacked monthly revenue by source for current calendar year
	if g.currentGraph == GraphLinear {
		// Try rendering via tui.chart to PNG and show inline (only if supported); fallback to ASCII
		if supportsInlineImages() {
			if err := g.ensureRevenueData(); err == nil {
				png, err := renderRevenueChartPNG(g.revData, 1600, 760)
				if err == nil && png != "" {
					if img := renderInlineImage(png); img != "" {
						return img
					}
				}
			}
		}
		stack, err := g.renderRevenueStacked()
		if err != nil {
			return lipgloss.NewStyle().Foreground(theme.Accent).Render(fmt.Sprintf("Error: %v", err))
		}
		return stack
	}

	// Graph two: B/S — two line charts stacked (Assets and Liabilities) for Calendar Year 2025
	if g.currentGraph == GraphQuadratic {
		bs, err := g.renderBalanceSheetLines()
		if err != nil {
			return lipgloss.NewStyle().Foreground(theme.Accent).Render(fmt.Sprintf("Error: %v", err))
		}
		return bs
	}

	// Graph three: Liabilities — top 4 YTD lines (Jan 1 to current month-end) rendered via headless Chrome
	img := g.renderLiabilitiesTop4()
	if strings.TrimSpace(img) == "" {
		return lipgloss.NewStyle().Foreground(theme.Accent).Render("No data for liabilities")
	}
	return img
}

// renderLiabilitiesTop4 builds a multi-series line chart (top 4 liabilities accounts by YTD sum)
// with x-axis from Jan 1 to current month-end, using headless Chrome to render a PNG.
// Returns an inline image escape sequence if supported; otherwise returns an empty string.
func (g *graphView) renderLiabilitiesTop4() string {
	year := time.Now().Year()
	months, names, series, err := g.ensureLiabilitiesTop4Data(year)
	if err != nil {
		return lipgloss.NewStyle().Foreground(theme.Accent).Render(fmt.Sprintf("Error: %v", err))
	}
	// Build SVG HTML (offline) and screenshot via headless chrome
	// Determine approximate pixel dimensions for readability
	pxW := 1400
	pxH := 700
	html := buildLineChartHTML(names, series, months, pxW, pxH, fmt.Sprintf("Liabilities — Top 4 YTD %d", year), "Month")
	png, err := renderHTMLChartPNG(html, "puffin_liab_top4", 15*time.Second)
	if err != nil || png == "" {
		return lipgloss.NewStyle().Foreground(theme.Accent).Render("Failed to render liabilities chart")
	}
	return renderInlineImage(png)
}

// ensureLiabilitiesTop4Data returns month labels (Jan..current), top 4 account names, and their series data
// by parsing `hledger balance type:l --monthly` from Jan 1 to the first day of next month (inclusive of current month-end).
func (g *graphView) ensureLiabilitiesTop4Data(year int) ([]string, []string, [][]float64, error) {
	// End date: first day of next month to include current month
	now := time.Now()
	nextMonthStart := time.Date(now.Year(), now.Month()+1, 1, 0, 0, 0, 0, time.Local)
	opts := hledger.NewOptions().
		WithStartDate(fmt.Sprintf("%04d-01-01", year)).
		WithEndDate(nextMonthStart.Format("2006-01-02")).
		WithPeriod(hledger.PeriodMonthly).
		WithPretty(true).
		WithLayout(hledger.LayoutBare).
		WithOutputCSV(true)

	args := []string{"hledger", "balance", "type:l"}
	args = append(args, opts.Build()...)
	if lf := os.Getenv("HLEDGER_FILE"); strings.TrimSpace(lf) != "" {
		args = append(args, "-f", lf)
	}
	cmd := exec.Command(args[0], args[1:]...)
	out, err := cmd.CombinedOutput()
	if err != nil {
		return nil, nil, nil, fmt.Errorf("%v: %s", err, string(out))
	}
	rows, err := parseCSV(bytes.NewReader(out))
	if err != nil {
		return nil, nil, nil, fmt.Errorf("parse csv: %w", err)
	}
	if len(rows) < 2 { return nil, nil, nil, fmt.Errorf("no liabilities data") }

	header := rows[0]
	if len(header) < 2 { return nil, nil, nil, fmt.Errorf("unexpected header") }
	monthStart := 1
	monthEnd := len(header)
	if strings.Contains(strings.ToLower(strings.TrimSpace(header[len(header)-1])), "total") {
		monthEnd--
	}
	months := make([]string, 0, monthEnd-monthStart)
	for i := monthStart; i < monthEnd; i++ {
		// Convert header date to month label if possible, else keep as-is
		label := header[i]
		if ts, err := time.Parse("2006-01-02", label); err == nil {
			label = ts.Format("Jan")
		}
		months = append(months, label)
	}

	// Accumulate per-account series
	type acc struct {
		name string
		sum  float64
		vals []float64
	}
	var accounts []acc
	for _, r := range rows[1:] {
		if len(r) < monthEnd { continue }
		name := strings.TrimSpace(r[0])
		if strings.EqualFold(name, "total") { continue }
		vals := make([]float64, 0, monthEnd-monthStart)
		var s float64
		for i := monthStart; i < monthEnd; i++ {
			v := parseAmount(r[i])
			if v < 0 { v = -v }
			vals = append(vals, v)
			s += v
		}
		if s == 0 { continue }
		accounts = append(accounts, acc{name: name, sum: s, vals: vals})
	}
	if len(accounts) == 0 { return nil, nil, nil, fmt.Errorf("no nonzero liabilities") }
	// Sort by YTD sum desc and pick top 4
	sort.Slice(accounts, func(i, j int) bool { return accounts[i].sum > accounts[j].sum })
	if len(accounts) > 4 { accounts = accounts[:4] }

	names := make([]string, 0, len(accounts))
	series := make([][]float64, 0, len(accounts))
	for _, a := range accounts {
		// Use leaf name for readability
		leaf := a.name
		if idx := strings.LastIndex(leaf, ":"); idx != -1 && idx+1 < len(leaf) {
			leaf = leaf[idx+1:]
		}
		names = append(names, leaf)
		series = append(series, a.vals)
	}
	return months, names, series, nil
}

func (g *graphView) IsShowingGraph() bool {
	return g.showGraph
}

// ---------------- Revenue stacked bar implementation ----------------

type revenueData struct {
	year    int
	months  []string
	top3    [][]float64 // [3][months] top 1,2,3 values per month
	other   []float64   // [months] other values per month
	yMax    float64
	loaded  bool
}

type legendItem struct {
	label string
	color lipgloss.Color
}

func (rd *revenueData) legendItems() []legendItem {
	// Generic legend: Top 1/2/3 and Other
	return []legendItem{
		{label: "Top 1", color: lipgloss.Color("#FFA500")}, // orange
		{label: "Top 2", color: lipgloss.Color("#1E90FF")}, // blue
		{label: "Top 3", color: lipgloss.Color("#32CD32")}, // green
		{label: "other", color: lipgloss.Color("#808080")}, // grey
	}
}

func (g *graphView) ensureRevenueData() error {
	year := time.Now().Year()
	if g.revData != nil && g.revData.loaded && g.revData.year == year {
		return nil
	}

	// Build hledger command similar to other views: balance type:r, monthly, csv, bare layout
	opts := hledger.NewOptions().
		WithStartDate(fmt.Sprintf("%04d-01-01", year)).
		WithEndDate(fmt.Sprintf("%04d-12-31", year)).
		WithPeriod(hledger.PeriodMonthly).
		WithPretty(true).
		WithLayout(hledger.LayoutBare).
		WithAccountDrop(1).
		WithOutputCSV(true)

	args := []string{"hledger", "balance", "type:r"}
	args = append(args, opts.Build()...)

	// Respect HLEDGER_FILE if present
	if lf := os.Getenv("HLEDGER_FILE"); strings.TrimSpace(lf) != "" {
		args = append(args, "-f", lf)
	}

	cmd := exec.Command(args[0], args[1:]...)
	out, err := cmd.CombinedOutput()
	if err != nil {
		return fmt.Errorf("%v: %s", err, string(out))
	}

	// Parse CSV
	rows, err := parseCSV(bytes.NewReader(out))
	if err != nil {
		return fmt.Errorf("parse csv: %w", err)
	}
	if len(rows) < 2 {
		return fmt.Errorf("no revenue data")
	}

	header := rows[0]
	if len(header) < 2 {
		return fmt.Errorf("unexpected header")
	}

	// Determine month columns: everything after first column, drop trailing Total if present
	monthStart := 1
	monthEnd := len(header)
	last := strings.ToLower(strings.TrimSpace(header[len(header)-1]))
	if strings.Contains(last, "total") {
		monthEnd--
	}
	months := make([]string, 0, monthEnd-monthStart)
	for i := monthStart; i < monthEnd; i++ {
		months = append(months, header[i])
	}

	// Collect per-account monthly values
	type acc struct {
		name string
		vals []float64
	}
	var accounts []acc
	for _, r := range rows[1:] {
		if len(r) < monthEnd {
			continue
		}
		name := strings.TrimSpace(r[0])
		if strings.EqualFold(strings.TrimSpace(name), "total") {
			continue
		}
		vals := make([]float64, 0, monthEnd-monthStart)
		var stotal float64
		for i := monthStart; i < monthEnd; i++ {
			v := parseAmount(r[i])
			if v < 0 {
				v = -v // revenue often negative in hledger; plot positive dollars
			}
			vals = append(vals, v)
			stotal += v
		}
		if stotal == 0 {
			continue
		}
		accounts = append(accounts, acc{name: name, vals: vals})
	}
	if len(accounts) == 0 {
		return fmt.Errorf("no nonzero revenue sources")
	}

	monthsCount := len(months)
	top1 := make([]float64, monthsCount)
	top2 := make([]float64, monthsCount)
	top3 := make([]float64, monthsCount)
	other := make([]float64, monthsCount)

	for m := 0; m < monthsCount; m++ {
		// Gather values for this month
		vals := make([]float64, 0, len(accounts))
		for _, a := range accounts {
			vals = append(vals, a.vals[m])
		}
		sort.Slice(vals, func(i, j int) bool { return vals[i] > vals[j] })
		if len(vals) > 0 {
			top1[m] = vals[0]
		}
		if len(vals) > 1 {
			top2[m] = vals[1]
		}
		if len(vals) > 2 {
			top3[m] = vals[2]
		}
		var sum float64
		for _, v := range vals {
			sum += v
		}
		other[m] = sum - top1[m] - top2[m] - top3[m]
		if other[m] < 0 {
			other[m] = 0
		}
	}

	// yMax is max monthly total across top1+top2+top3+other
	var yMax float64
	for m := 0; m < monthsCount; m++ {
		s := top1[m] + top2[m] + top3[m] + other[m]
		if s > yMax {
			yMax = s
		}
	}
	if yMax == 0 {
		yMax = 1
	}

	g.revData = &revenueData{
		year:   year,
		months: months,
		top3:   [][]float64{top1, top2, top3},
		other:  other,
		yMax:   yMax,
		loaded: true,
	}
	return nil
}

func parseAmount(s string) float64 {
	s = strings.TrimSpace(s)
	if s == "" {
		return 0
	}
	neg := false
	// Handle (123.45) style negatives
	if strings.HasPrefix(s, "(") && strings.HasSuffix(s, ")") {
		neg = true
		s = strings.TrimPrefix(strings.TrimSuffix(s, ")"), "(")
	}
	// Remove common currency symbols and commas
	replacer := strings.NewReplacer(",", "", "$", "", "€", "", "£", "", "USD", "", "EUR", "", "%", "")
	s = replacer.Replace(s)
	s = strings.TrimSpace(s)
	if s == "" || s == "-" {
		return 0
	}
	v, err := strconv.ParseFloat(s, 64)
	if err != nil {
		return 0
	}
	if neg {
		v = -v
	}
	return v
}

func (g *graphView) renderRevenueStacked() (string, error) {
	if err := g.ensureRevenueData(); err != nil {
		return "", err
	}
	rd := g.revData
	// Drawing parameters
	height := 20
	barWidth := 2
	gap := 1
	monthsCount := len(rd.months)
	if monthsCount == 0 {
		return "", fmt.Errorf("no months to display")
	}
	// Compute normalized heights for each segment per month
	colors := []lipgloss.Color{
		lipgloss.Color("#FFA500"), // orange Top 1
		lipgloss.Color("#1E90FF"), // blue Top 2
		lipgloss.Color("#32CD32"), // green Top 3
		lipgloss.Color("#808080"), // grey Other
	}
	labels := []string{"Top 1", "Top 2", "Top 3", "other"}

	// Precompute heights for Top 1..3 and Other
	heights := make([][]int, len(labels)) // [series][month]
	for si := 0; si < 3; si++ { // top1..3
		vals := rd.top3[si]
		mh := make([]int, monthsCount)
		for m := 0; m < monthsCount; m++ {
			v := vals[m]
			h := 0
			if rd.yMax > 0 {
				h = int(math.Round((v / rd.yMax) * float64(height)))
			}
			if v > 0 && h == 0 {
				h = 1
			}
			mh[m] = h
		}
		heights[si] = mh
	}
	// Other
	mhOther := make([]int, monthsCount)
	for m := 0; m < monthsCount; m++ {
		v := rd.other[m]
		h := 0
		if rd.yMax > 0 {
			h = int(math.Round((v / rd.yMax) * float64(height)))
		}
		if v > 0 && h == 0 {
			h = 1
		}
		mhOther[m] = h
	}
	heights[3] = mhOther

	var lines []string
	for y := height; y >= 1; y-- {
		var row strings.Builder
		for m := 0; m < monthsCount; m++ {
			// Determine which segment occupies this y for month m
			cum := 0
			segColor := lipgloss.Color("")
			filled := false
			for si := 0; si < len(labels); si++ {
				cum += heights[si][m]
				if y <= cum && heights[si][m] > 0 {
					segColor = colors[si]
					filled = true
					break
				}
			}
			cell := strings.Repeat(" ", barWidth)
			if filled {
				cell = lipgloss.NewStyle().Foreground(segColor).Render(strings.Repeat("█", barWidth))
			}
			row.WriteString(cell)
			if m != monthsCount-1 {
				row.WriteString(strings.Repeat(" ", gap))
			}
		}
		lines = append(lines, row.String())
	}

	// X-axis
	axisWidth := monthsCount*barWidth + (monthsCount-1)*gap
	axis := strings.Repeat("─", axisWidth)
	lines = append(lines, axis)

	// Month labels
	year := rd.year
	var monthLabelRow strings.Builder
	for m := 1; m <= monthsCount; m++ {
		monName := time.Date(year, time.Month(m), 1, 0, 0, 0, 0, time.UTC).Format("Jan")
		label := monName
		if len(label) > barWidth {
			label = label[:barWidth]
		}
		if len(label) < barWidth {
			label = label + strings.Repeat(" ", barWidth-len(label))
		}
		monthLabelRow.WriteString(label)
		if m != monthsCount {
			monthLabelRow.WriteString(strings.Repeat(" ", gap))
		}
	}
	lines = append(lines, monthLabelRow.String())

	// Title and framing
	title := fmt.Sprintf("Revenue by Source — %d (stacked, monthly)", rd.year)
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
		Render("Use Up/Down to switch graphs, 'q' to return")

	return lipgloss.JoinVertical(
		lipgloss.Center,
		titleStyle.Render(title),
		graphStyle.Render(strings.Join(lines, "\n")),
		instructions,
	), nil
}

// renderInlineImage returns an iTerm2/Kitty inline-image escape sequence to display the PNG.
// If the terminal doesn't support it, returns an empty string.
func renderInlineImage(pngPath string) string {
	// iTerm2 inline image protocol: OSC 1337
	data, err := os.ReadFile(pngPath)
	if err != nil { return "" }
	b64 := base64.StdEncoding.EncodeToString(data)
	// Use inline=1; terminals decide placement. We avoid explicit sizing to keep layout simple.
	seq := fmt.Sprintf("\x1b]1337;File=size=%d;inline=1:%s\x07", len(data), b64)
	return seq
}

// supportsInlineImages returns true for terminals likely to support inline images
func supportsInlineImages() bool {
	// Env override: PUFFIN_INLINE_IMAGES=1|true to force on, 0|false to force off
	if v := strings.ToLower(strings.TrimSpace(os.Getenv("PUFFIN_INLINE_IMAGES"))); v != "" {
		if v == "1" || v == "true" || v == "yes" || v == "on" { return true }
		if v == "0" || v == "false" || v == "no" || v == "off" { return false }
	}

	tp := strings.ToLower(os.Getenv("TERM_PROGRAM"))
	term := strings.ToLower(os.Getenv("TERM"))
	// iTerm2
	if strings.Contains(tp, "iterm") {
		return true
	}
	// Warp
	if strings.Contains(tp, "warp") {
		return true
	}
	// Kitty protocol
	if strings.Contains(term, "kitty") {
		return true
	}
	return false
}

// ---------------- B/S two-line charts implementation ----------------

type lineSeries struct {
	name  string
	vals  []float64
	color lipgloss.Color
}

// renderBalanceSheetLines renders two stacked line charts:
// Top: Assets accounts (bank:09, bank:chase)
// Bottom: Liabilities accounts (0335, 6154)
func (g *graphView) renderBalanceSheetLines() (string, error) {
	year := 2025 // Calendar Year 2025 as requested

	// Define accounts
	assetAccounts := []string{"assets:bank:09", "assets:bank:chase"}
	liabAccounts := []string{"liabilities:0335", "liabilities:6154"}

	monthsA, seriesA, err := g.fetchMonthlySeries(year, assetAccounts)
	if err != nil {
		return "", err
	}
	_, seriesL, err := g.fetchMonthlySeries(year, liabAccounts)
	if err != nil {
		return "", err
	}

	// Choose colors for lines
	if len(seriesA) == 2 {
		seriesA[0].color = lipgloss.Color("#1E90FF") // blue
		seriesA[1].color = lipgloss.Color("#FFA500") // orange
	}
	if len(seriesL) == 2 {
		seriesL[0].color = lipgloss.Color("#FF00FF") // magenta
		seriesL[1].color = lipgloss.Color("#00CED1") // dark turquoise
	}

	// Render charts
	chartWidth := 64
	chartHeight := 12

	titleStyle := lipgloss.NewStyle().Bold(true).Foreground(theme.Accent).Align(lipgloss.Center).Margin(1)
	sectionTitle := titleStyle.Render("B/S")

	topLegend := g.renderLegend([]lineSeries{seriesA[0], seriesA[1]})
	topChart := g.renderLineChart("Assets — 2025", monthsA, []lineSeries{seriesA[0], seriesA[1]}, chartWidth, chartHeight)

	bottomLegend := g.renderLegend([]lineSeries{seriesL[0], seriesL[1]})
	bottomChart := g.renderLineChart("Liabilities — 2025", monthsA, []lineSeries{seriesL[0], seriesL[1]}, chartWidth, chartHeight)

	chartStyle := lipgloss.NewStyle().
		Border(lipgloss.RoundedBorder()).
		BorderForeground(theme.PrimaryForeground).
		Padding(1)

	instructions := lipgloss.NewStyle().Foreground(theme.PrimaryForeground).Align(lipgloss.Center).Render("Use Up/Down to switch graphs, 'q' to return")

	left := chartStyle.Render(lipgloss.JoinVertical(lipgloss.Left, topLegend, topChart))
	right := chartStyle.Render(lipgloss.JoinVertical(lipgloss.Left, bottomLegend, bottomChart))
	sideBySide := lipgloss.JoinHorizontal(lipgloss.Top, left, right)

	return lipgloss.JoinVertical(
		lipgloss.Center,
		sectionTitle,
		sideBySide,
		instructions,
	), nil
}

// fetchMonthlySeries returns the months and per-account monthly balances (absolute values) for the given year.
func (g *graphView) fetchMonthlySeries(year int, accounts []string) ([]string, []lineSeries, error) {
	var months []string
	series := make([]lineSeries, 0, len(accounts))
	for _, acct := range accounts {
		m, vals, err := g.getMonthlyBalances(year, acct)
		if err != nil {
			return nil, nil, err
		}
		if len(months) == 0 {
			months = m
		}
		name := acct
		if idx := strings.LastIndex(acct, ":"); idx != -1 && idx+1 < len(acct) {
			name = acct[idx+1:]
		}
		series = append(series, lineSeries{name: name, vals: vals})
	}
	return months, series, nil
}

// getMonthlyBalances queries hledger for a single account and returns month labels and absolute balances for each month of the year.
func (g *graphView) getMonthlyBalances(year int, account string) ([]string, []float64, error) {
	opts := hledger.NewOptions().
		WithStartDate(fmt.Sprintf("%04d-01-01", year)).
		WithEndDate(fmt.Sprintf("%04d-12-31", year)).
		WithPeriod(hledger.PeriodMonthly).
		WithPretty(true).
		WithLayout(hledger.LayoutBare).
		WithOutputCSV(true)

	args := []string{"hledger", "balance", account}
	args = append(args, opts.Build()...)

	// Respect HLEDGER_FILE if present
	if lf := os.Getenv("HLEDGER_FILE"); strings.TrimSpace(lf) != "" {
		args = append(args, "-f", lf)
	}

	cmd := exec.Command(args[0], args[1:]...)
	out, err := cmd.CombinedOutput()
	if err != nil {
		return nil, nil, fmt.Errorf("%v: %s", err, string(out))
	}

	rows, err := parseCSV(bytes.NewReader(out))
	if err != nil {
		return nil, nil, fmt.Errorf("parse csv: %w", err)
	}
	if len(rows) < 2 {
		return nil, nil, fmt.Errorf("no data for %s", account)
	}

	header := rows[0]
	if len(header) < 2 {
		return nil, nil, fmt.Errorf("unexpected header for %s", account)
	}
	monthStart := 1
	monthEnd := len(header)
	last := strings.ToLower(strings.TrimSpace(header[len(header)-1]))
	if strings.Contains(last, "total") {
		monthEnd--
	}

	months := make([]string, 0, monthEnd-monthStart)
	for i := 1; i <= (monthEnd-monthStart); i++ {
		months = append(months, time.Date(year, time.Month(i), 1, 0, 0, 0, 0, time.UTC).Format("Jan"))
	}

	vals := make([]float64, monthEnd-monthStart)
	for _, r := range rows[1:] {
		if len(r) < monthEnd {
			continue
		}
		if strings.EqualFold(strings.TrimSpace(r[0]), "total") {
			continue
		}
		for i := monthStart; i < monthEnd; i++ {
			v := parseAmount(r[i])
			if v < 0 { // plot absolute values for readability
				v = -v
			}
			vals[i-monthStart] += v
		}
	}

	return months, vals, nil
}

// renderLegend renders a simple legend from series names and colors.
func (g *graphView) renderLegend(series []lineSeries) string {
	items := make([]string, 0, len(series))
	for _, s := range series {
		colorStyle := lipgloss.NewStyle().Foreground(s.color)
		swatch := colorStyle.Render("■")
		items = append(items, lipgloss.JoinHorizontal(lipgloss.Left, swatch, " ", s.name))
	}
	legendTitle := sectionTitleStyle.Copy().Render("KEY")
	return lipgloss.JoinHorizontal(lipgloss.Left, legendTitle, "  ", strings.Join(items, "   "))
}

// renderLineChart draws a simple multi-series line chart with month labels.
func (g *graphView) renderLineChart(title string, months []string, series []lineSeries, width, height int) string {
	if len(months) == 0 || len(series) == 0 {
		return lipgloss.NewStyle().Foreground(theme.Accent).Render("No data")
	}
	if width < 12 {
		width = 12
	}
	if height < 6 {
		height = 6
	}

	// Determine y-range across all series
	var yMax float64
	for _, s := range series {
		for _, v := range s.vals {
			if v > yMax { yMax = v }
		}
	}
	if yMax == 0 { yMax = 1 }

	// Build grid
	grid := make([][]rune, height)
	for i := range grid {
		grid[i] = make([]rune, width)
		for j := range grid[i] { grid[i][j] = ' ' }
	}

	n := len(months)
	// Map months to x positions across width
	xpos := make([]int, n)
	for i := 0; i < n; i++ {
		xpos[i] = int(math.Round(float64(i) / float64(n-1) * float64(width-1)))
	}

	// Plot points for each series
	markers := []rune{'●', '×', '•', '■'}
	for si, s := range series {
		mk := markers[si%len(markers)]
		col := lipgloss.NewStyle().Foreground(s.color)
		for i := 0; i < n; i++ {
			v := s.vals[i]
			yNorm := 0.0
			if yMax > 0 { yNorm = v / yMax }
			y := int(math.Round((1.0 - yNorm) * float64(height-1)))
			if y < 0 { y = 0 }
			if y >= height { y = height-1 }
			grid[y][xpos[i]] = mk
			// Colorize in-place by wrapping the single rune column when converting to string
			_ = col
		}
	}

	// Convert grid rows to strings; then apply color by replacing markers per series
	lines := make([]string, height)
	for i := 0; i < height; i++ {
		lines[i] = string(grid[i])
	}
	// Apply colors: replace each marker with colored versions
	for si := range series {
		mk := string(markers[si%len(markers)])
		col := lipgloss.NewStyle().Foreground(series[si].color)
		for i := 0; i < height; i++ {
			if strings.Contains(lines[i], mk) {
				lines[i] = strings.ReplaceAll(lines[i], mk, col.Render(mk))
			}
		}
	}

	// Month labels row
	var labelRow strings.Builder
	for i := 0; i < n; i++ {
		label := months[i]
		if len(label) > 3 { label = label[:3] }
		// Place label centered under its x position if possible
		pos := xpos[i]
		// Fill spaces until pos
		for labelRow.Len() < pos {
			labelRow.WriteByte(' ')
		}
		labelRow.WriteString(label)
	}

	titleStyle := lipgloss.NewStyle().Bold(true).Foreground(theme.PrimaryForeground)
	graphStyle := lipgloss.NewStyle().Border(lipgloss.NormalBorder()).BorderForeground(theme.PrimaryForeground).Padding(0)

	return lipgloss.JoinVertical(
		lipgloss.Left,
		titleStyle.Render(title),
		graphStyle.Render(strings.Join(lines, "\n")),
		labelRow.String(),
	)
}
