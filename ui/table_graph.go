package ui

import (
	"bytes"
	"fmt"
	"log"
	"os"
	"os/exec"
	"strconv"
	"strings"
	"time"
	"math"

	"github.com/charmbracelet/bubbles/viewport"
	tea "github.com/charmbracelet/bubbletea"
	"github.com/charmbracelet/lipgloss"
	"github.com/guptarohit/asciigraph"
	"github.com/siddhantac/hledger"
)

type TableGraph struct {
	size         size
	tableSize    size
	viewportSize size

	name      string
	table     *Table
	viewport  viewport.Model
	id        int
	locked    bool
	cmd       func(int, hledger.Options) content
	showGraph bool
	cmdType   cmdType
}

// extractGraphSeriesFromRow returns only the numeric series cells corresponding to date columns
// (e.g., YYYY-MM-DD or YYYY-MM). It ignores trailing aggregate columns like Q1, Q2, YTD.
func (t *TableGraph) extractGraphSeriesFromRow(row []string) []string {
	if row == nil || len(row) == 0 || t.table == nil || len(t.table.columns) == 0 {
		return nil
	}
	// Build header titles from current columns
	titles := make([]string, len(t.table.columns))
	for i, c := range t.table.columns {
		titles[i] = c.Title
	}
	var series []string
	for i, title := range titles {
		if isDateHeader(title) {
			if i < len(row) {
				series = append(series, row[i])
			}
		}
	}
	if len(series) == 0 && len(row) > 2 {
		// Fallback to previous behavior
		return row[2:]
	}
	return series
}

// isDateHeader returns true if the string looks like a date header (YYYY-MM-DD or YYYY-MM)
func isDateHeader(s string) bool {
	s = strings.TrimSpace(s)
	if s == "" { return false }
	if _, err := time.Parse("2006-01-02", s); err == nil {
		return true
	}
	if len(s) >= 7 {
		if _, err := time.Parse("2006-01", s[:7]); err == nil {
			return true
		}
	}
	// Also accept plain year headers
	if len(s) == 4 {
		allDigits := true
		for _, r := range s {
			if r < '0' || r > '9' { allDigits = false; break }
		}
		if allDigits { return true }
	}
	return false
}

func newTableGraph(id int, name string, locked bool, cmd func(int, hledger.Options) content, cmdType cmdType, dataTransformers []dataTransformer) *TableGraph {
	showGraph := true
	if locked {
		showGraph = false
	}
	return &TableGraph{
		id:        id,
		name:      name,
		locked:    locked,
		cmd:       cmd,
		cmdType:   cmdType,
		table:     newTable(name, nil, id, cmd, locked, cmdType, dataTransformers),
		viewport:  viewport.New(10, 10),
		showGraph: showGraph,
	}
}

func (t *TableGraph) Run(options hledger.Options) tea.Cmd {
	return func() tea.Msg {
		return t.cmd(t.id, options)
	}
}

func (t *TableGraph) log(msg string) {
	log.Printf("%s(%d): %s", t.name, t.id, msg)
}

func (t *TableGraph) Type() cmdType { return t.cmdType }
func (t *TableGraph) Locked() bool  { return t.locked }
func (t *TableGraph) IsReady() bool { return t.table.IsReady() }
func (t *TableGraph) SetUnready()   { t.table.SetUnready() }
func (t *TableGraph) SetContent(gc content) {
	if gc.id != t.id {
		return
	}
	t.log("setting content")

	t.table.SetContent(gc)
	// Refresh the graph content based on current state (including Assets weekly view)
	t.updateGraph()
}

func (t *TableGraph) Init() tea.Cmd {
	t.SetUnready()
	return nil
}

func (t *TableGraph) calculateTableHeight() int {
	if t.locked || !t.showGraph {
		return t.size.Height
	}

	// use subtraction instead of percent to avoid rounding issues
	// which can cause differences of 1 line
	return t.size.Height - t.viewportSize.Height
}

func (t *TableGraph) Update(msg tea.Msg) (tea.Model, tea.Cmd) {
	t.viewport.Update(msg)

	switch msg := msg.(type) {
case tea.WindowSizeMsg:
			t.size = size{
				Width:  msg.Width,
				Height: msg.Height,
			}

			t.viewportSize = size{
				Width:  percent(t.size.Width, 99),
				// Increase graph viewport height by ~1.5x (25% -> 38%)
				Height: percent(t.size.Height, 38),
			}
			t.tableSize = size{
				Width:  t.size.Width,
				Height: t.calculateTableHeight(),
			}

			t.table.Update(tea.WindowSizeMsg(t.tableSize))
			t.viewport = viewport.New(t.viewportSize.Width, t.viewportSize.Height)

			t.log(fmt.Sprintf("windowSize: %s, tableSize: %s, viewportSize: %s", t.size, t.tableSize, t.viewportSize))

			// Refresh graph after resize (handles Assets weekly multi-series)
			t.updateGraph()

			return t, nil

case tea.KeyMsg:
		t.table.Update(msg)
		switch msg.String() {
case "g":
			if t.locked {
				return t, nil
			}

			t.showGraph = !t.showGraph
			t.tableSize.Height = t.calculateTableHeight()
			t.table.Update(tea.WindowSizeMsg(t.tableSize))

			// Update graph content when toggling visibility
			t.updateGraph()

			t.log(fmt.Sprintf("showGraph: %v, tableSize: %v, size: %v, graph: %v", t.showGraph, t.tableSize, t.size, t.viewportSize))
		case "up", "down":
			// After arrow navigation, refresh graph. For Assets, we show multi-series weekly independent of selection.
			t.updateGraph()
		}
	}

	return t, nil
}

func (t *TableGraph) View() string {
	if !t.locked && t.showGraph {
		return lipgloss.JoinVertical(
			lipgloss.Left,
			t.table.View(),
			t.viewport.View(),
		)
	}
	return t.table.View()
}

func (t *TableGraph) plotGraph(rows []float64, legend string) string {
	if rows == nil {
		return ""
	}
	graph := asciigraph.Plot(
		rows,
		asciigraph.SeriesColors(asciigraph.IndianRed),
		asciigraph.Height(t.viewportSize.Height-3),
		asciigraph.Width(t.viewportSize.Width),
		asciigraph.SeriesLegends(legend),
	)
	return graph
}

// updateGraph refreshes the viewport graph depending on current report and state.
// For Assets report, it renders a weekly multi-series graph for the current year
// across specific accounts. Otherwise, it renders the selected row series.
func (t *TableGraph) updateGraph() {
	if !t.table.IsReady() {
		return
	}
	if t.table.NumRows() < 1 {
		t.viewport.SetContent("")
		return
	}
	if t.locked || !t.showGraph {
		return
	}
	// Defer heavy work until we have received an initial WindowSizeMsg
	if t.viewportSize.Width <= 0 || t.viewportSize.Height <= 0 {
		return
	}
	if strings.EqualFold(strings.TrimSpace(t.name), "assets") {
		if g := t.assetsWeeklyGraph(); g != "" {
			t.viewport.SetContent(g)
		}
		return
	}
	if strings.EqualFold(strings.TrimSpace(t.name), "revenue") {
		if g := t.revenueMonthlyGraph(); g != "" {
			t.viewport.SetContent(g)
		}
		return
	}
	// Default: plot the selected row
	row := t.table.SelectedRow()
	series := t.extractGraphSeriesFromRow(row)
	t.viewport.SetContent(t.plotGraph(strSliceToNumbers(series), row[0]))
}

// assetsWeeklyGraph builds a weekly (current year) multi-series line chart for key asset accounts.
func (t *TableGraph) assetsWeeklyGraph() string {
	year := time.Now().Year()
	accounts := []string{
		"assets:bank:09",
		"assets:bank:chase",
		"assets:bank:first",
		"assets:bank:wffedcu",
		"assets:schwab:roth",
	}

	series := make([][]float64, 0, len(accounts))
	legends := make([]string, 0, len(accounts))
	var labels []string
	for _, acct := range accounts {
		labs, vals, err := t.getWeeklyBalances(year, acct)
		if err != nil || len(vals) == 0 {
			continue
		}
		if len(labels) == 0 {
			labels = labs
		}
		// Ensure all series have equal length by trimming/padding
		if len(vals) > len(labels) { vals = vals[:len(labels)] }
		if len(vals) < len(labels) {
			pad := make([]float64, len(labels)-len(vals))
			vals = append(vals, pad...)
		}
		series = append(series, vals)
		legend := acct
		if idx := strings.LastIndex(acct, ":"); idx != -1 && idx+1 < len(acct) {
			legend = acct[idx+1:]
		}
		legends = append(legends, legend)
	}
	if len(series) == 0 {
		return ""
	}

	// If terminal supports inline images, render via headless Chrome (Toast UI line chart)
	if supportsInlineImages() {
		// Approximate pixel sizing from terminal cells
		pxW := t.viewportSize.Width * 9
		// Shrink image height so bottom labels are visible within viewport; then 1.5x as requested
		pxPerRow := 18
		vh := t.viewportSize.Height
		offset := 7
		linesForImg := vh
		if vh > offset+3 { linesForImg = vh - offset }
		pxH := linesForImg * pxPerRow
		if pxW < 800 { pxW = 800 }
		if pxH < 240 { pxH = 240 }
		// Build month labels aligned to weekly points: label only the first week of each month
		monthCats := make([]string, len(labels))
		var prevMonth time.Month
		var prevYear int
		for i, lab := range labels {
			if ts, err := time.Parse("2006-01-02", strings.TrimSpace(lab)); err == nil {
				if i == 0 || ts.Month() != prevMonth || ts.Year() != prevYear {
					monthCats[i] = ts.Format("Jan")
					prevMonth = ts.Month()
					prevYear = ts.Year()
				}
			}
		}
		// Fallback: if no months detected, place Jan..Dec at ~evenly spaced positions
		nonEmpty := 0
		for _, s := range monthCats { if strings.TrimSpace(s) != "" { nonEmpty++ } }
		if nonEmpty == 0 && len(labels) >= 12 {
			months12 := []string{"Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"}
			for m := 0; m < 12; m++ {
				idx := int(math.Round(float64(m) / 11.0 * float64(len(labels)-1)))
				monthCats[idx] = months12[m]
			}
		}
		// First try Go-based PNG (no Chrome)
		if png2, err := renderGoLineChartPNG(legends, series, monthCats, pxW, pxH, "Assets — Weekly", "Month", 0, 2100, 300); err == nil && png2 != "" {
			if img := renderInlineImage(png2); img != "" { return img }
		}
		// Then try Chrome-based HTML -> PNG
		html := buildLineChartHTMLWithScale(legends, series, monthCats, pxW, pxH, "Assets — Weekly", "Month", 0, 2100, 300)
		if png, err := renderHTMLChartPNG(html, "puffin_assets", 15*time.Second); err == nil && png != "" {
			if img := renderInlineImage(png); img != "" { return img }
		}
		// fallthrough to ASCII if anything fails
	}

	// ASCII fallback: Render multi-series graph matching viewport height, with fixed Y scale and month labels
	height := t.viewportSize.Height - 5 // leave extra space for x-axis labels
	if height < 5 { height = 5 }
	width := t.viewportSize.Width
	if width < 10 { width = 60 }
	// Build month categories aligned to weekly points
	monthCats := make([]string, len(labels))
	var prevMonth time.Month
	var prevYear int
	for i, lab := range labels {
		if ts, err := time.Parse("2006-01-02", strings.TrimSpace(lab)); err == nil {
			if i == 0 || ts.Month() != prevMonth || ts.Year() != prevYear {
				monthCats[i] = ts.Format("Jan")
				prevMonth = ts.Month()
				prevYear = ts.Year()
			}
		}
	}
	// Fallback evenly spaced Jan..Dec if none detected
	nonEmpty := 0
	for _, s := range monthCats { if strings.TrimSpace(s) != "" { nonEmpty++ } }
	if nonEmpty == 0 && len(labels) >= 12 {
		months12 := []string{"Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"}
		for m := 0; m < 12; m++ {
			idx := int(math.Round(float64(m) / 11.0 * float64(len(labels)-1)))
			monthCats[idx] = months12[m]
		}
	}
	graph := renderMultiSeriesASCIIFixed(series, width, height, legends, 0, 2100, 300, monthCats)
	return graph
}

// renderMultiSeriesASCIIFixed draws a multi-series ASCII chart with fixed Y-range [yMin,yMax],
// tick labels every tickStep, and optional category labels (cats) placed under their x positions
// when non-empty. It returns the full chart string including a legend.
func renderMultiSeriesASCIIFixed(series [][]float64, width, height int, legends []string, yMin, yMax, tickStep float64, cats []string) string {
	if width < 12 { width = 12 }
	if height < 6 { height = 6 }
	// Colors for series
	legendColors := []lipgloss.Color{
		lipgloss.Color("#1E90FF"), // Blue
		lipgloss.Color("#00CED1"), // Cyan
		lipgloss.Color("#32CD32"), // Green
		lipgloss.Color("#FFD700"), // Yellow
		lipgloss.Color("#FF00FF"), // Magenta
	}
	// Legend line
	legendItems := make([]string, 0, len(legends))
	for i, name := range legends {
		style := lipgloss.NewStyle().Foreground(legendColors[i%len(legendColors)])
		legendItems = append(legendItems, lipgloss.JoinHorizontal(lipgloss.Left, style.Render("■"), " ", style.Render("-"), " ", style.Render(name)))
	}
	legendLine := "Legend: " + strings.Join(legendItems, "   ")

	// Prepare grid
	grid := make([][]rune, height)
	for i := range grid { grid[i] = make([]rune, width); for j := range grid[i] { grid[i][j] = ' ' } }
	// Map x positions over the longest series length
	n := 0
	for _, s := range series { if len(s) > n { n = len(s) } }
	if n <= 1 { return legendLine }
	xpos := make([]int, n)
	for i := 0; i < n; i++ { xpos[i] = int(math.Round(float64(i) / float64(n-1) * float64(width-1))) }
	// Y mapping
	yRange := yMax - yMin
	if yRange <= 0 { yRange = 1 }
	mapY := func(v float64) int {
		if v < yMin { v = yMin }
		if v > yMax { v = yMax }
		y := int(math.Round((1.0 - ((v-yMin)/yRange)) * float64(height-1)))
		if y < 0 { y = 0 }
		if y >= height { y = height-1 }
		return y
	}
	// Draw connecting lines first (so markers overlay them)
	drawLine := func(x0, y0, x1, y1 int) {
		dx := abs(x1 - x0)
		dy := -abs(y1 - y0)
		sx := -1
		if x0 < x1 { sx = 1 }
		sy := -1
		if y0 < y1 { sy = 1 }
		err := dx + dy
		x, y := x0, y0
		for {
			if x == x1 && y == y1 { break }
			e2 := 2 * err
			if e2 >= dy { err += dy; x += sx }
			if e2 <= dx { err += dx; y += sy }
			if x >= 0 && x < width && y >= 0 && y < height {
				if grid[y][x] == ' ' { grid[y][x] = '─' }
			}
		}
	}
	for _, s := range series {
		for i := 0; i < n-1 && i+1 < len(s); i++ {
			x0, y0 := xpos[i], mapY(s[i])
			x1, y1 := xpos[i+1], mapY(s[i+1])
			drawLine(x0, y0, x1, y1)
		}
	}
	// Now plot markers on top
	markers := []rune{'●','×','•','■'}
	for si, s := range series {
		mk := markers[si%len(markers)]
		for i := 0; i < n && i < len(s); i++ {
			y := mapY(s[i])
			grid[y][xpos[i]] = mk
		}
	}
	lines := make([]string, height)
	for i := 0; i < height; i++ { lines[i] = string(grid[i]) }

	// Build y-axis labels using fixed step
	labelForRow := make(map[int]string)
	maxLabelLen := 0
	if tickStep <= 0 { tickStep = (yMax - yMin) / 5 }
	for val := yMin; val <= yMax+1e-6; val += tickStep {
		row := mapY(val)
		lab := fmt.Sprintf("%.0f", val)
		labelForRow[row] = lab
		if l := len(lab); l > maxLabelLen { maxLabelLen = l }
	}
	labelWidth := max(4, maxLabelLen)
	graphStyle := lipgloss.NewStyle().Border(lipgloss.NormalBorder()).BorderForeground(lipgloss.Color("240")).Padding(0, 0, 0, 0)
	bordered := graphStyle.Render(strings.Join(lines, "\n"))
	borderLines := strings.Split(bordered, "\n")
	finalLines := make([]string, 0, len(borderLines))
	for i, bl := range borderLines {
		if i == 0 || i == len(borderLines)-1 {
			finalLines = append(finalLines, strings.Repeat(" ", labelWidth+3)+bl)
			continue
		}
		row := i - 1
		if lab, ok := labelForRow[row]; ok {
			finalLines = append(finalLines, fmt.Sprintf("%*s ┤ %s", labelWidth, lab, bl))
		} else {
			finalLines = append(finalLines, fmt.Sprintf("%*s │ %s", labelWidth, "", bl))
		}
	}
	// Build x labels from cats
	xIndent := strings.Repeat(" ", labelWidth+3)
	var xRow strings.Builder
	for i := 0; i < n && i < len(cats); i++ {
		lab := strings.TrimSpace(cats[i])
		if lab == "" { continue }
		pos := xpos[i]
		for xRow.Len() < pos { xRow.WriteByte(' ') }
		xRow.WriteString(lab)
	}

	return legendLine + "\n" + strings.Join(finalLines, "\n") + "\n" + xIndent + xRow.String()
}

// helper abs for ints
func abs(x int) int { if x < 0 { return -x }; return x }
func (t *TableGraph) revenueMonthlyGraph() string {
	year := time.Now().Year()
	// Target accounts (case-insensitive match names used by the user)
	accounts := []string{"Durham", "driving", "insurance"}
	series := make([][]float64, 0, len(accounts))
	legends := make([]string, 0, len(accounts))
	// Always show 12 calendar months (Jan..Dec)
	months := []string{"Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"}
	endDate := fmt.Sprintf("%04d-12-31", year)
	for _, acct := range accounts {
		_, vals, err := t.getMonthlyBalancesRange(year, acct, endDate)
		if err != nil || len(vals) == 0 {
			continue
		}
		// Trim/pad to exactly 12 entries
		if len(vals) > 12 { vals = vals[:12] }
		if len(vals) < 12 {
			pad := make([]float64, 12-len(vals))
			vals = append(vals, pad...)
		}
		series = append(series, vals)
		legend := acct
		if idx := strings.LastIndex(acct, ":"); idx != -1 && idx+1 < len(acct) { legend = acct[idx+1:] }
		// Title-case legend for aesthetics
		if len(legend) > 0 { legend = strings.ToUpper(legend[:1]) + legend[1:] }
		legends = append(legends, legend)
	}
	if len(series) == 0 { return "" }

	if supportsInlineImages() {
		pxW := t.viewportSize.Width * 9
		// Shrink image height so bottom labels are visible within viewport
		pxPerRow := 18
		vh := t.viewportSize.Height
		offset := 7
		linesForImg := vh
		if vh > offset+3 { linesForImg = vh - offset }
		pxH := linesForImg * pxPerRow
		if pxW < 800 { pxW = 800 }
		if pxH < 240 { pxH = 240 }
		// First try Go-based PNG (no Chrome)
		if png2, err := renderGoLineChartPNG(legends, series, months, pxW, pxH, "Revenue — Monthly", "Month", 0, 0, 0); err == nil && png2 != "" {
			if img := renderInlineImage(png2); img != "" { return img }
		}
		// Then try Chrome-based HTML -> PNG
		html := buildLineChartHTML(legends, series, months, pxW, pxH, "Revenue — Monthly", "Month")
		if png, err := renderHTMLChartPNG(html, "puffin_revenue_ms", 15*time.Second); err == nil && png != "" {
			if img := renderInlineImage(png); img != "" { return img }
		}
	}

	// ASCII fallback
	height := t.viewportSize.Height - 3
	if height < 5 { height = 5 }
	width := t.viewportSize.Width
	if width < 10 { width = 60 }
	// Trim/pad all series to 12 to avoid uneven lengths
	for i := range series {
		if len(series[i]) > 12 { series[i] = series[i][:12] }
		if len(series[i]) < 12 {
			pad := make([]float64, 12-len(series[i]))
			series[i] = append(series[i], pad...)
		}
	}
	graph := asciigraph.PlotMany(
		series,
		asciigraph.Height(height),
		asciigraph.Width(width),
		asciigraph.SeriesColors(
			asciigraph.Blue,
			asciigraph.Cyan,
			asciigraph.Green,
		),
	)
	legendColors := []lipgloss.Color{
		lipgloss.Color("#1E90FF"),
		lipgloss.Color("#00CED1"),
		lipgloss.Color("#32CD32"),
	}
	legendItems := make([]string, 0, len(legends))
	for i, name := range legends {
		style := lipgloss.NewStyle().Foreground(legendColors[i%len(legendColors)])
		legendItems = append(legendItems, lipgloss.JoinHorizontal(lipgloss.Left, style.Render("■"), " ", style.Render("-"), " ", style.Render(name)))
	}
	legendLine := "Legend: " + strings.Join(legendItems, "   ")
	// Append a simple month label row for ASCII fallback
	labelRow := strings.Join(months, " ")
	return legendLine + "\n" + graph + "\n" + labelRow
}

// getWeeklyBalances returns absolute weekly balances for the given account over the specified year.
func (t *TableGraph) getWeeklyBalances(year int, account string) ([]string, []float64, error) {
	start := fmt.Sprintf("%04d-01-01", year)
	end := fmt.Sprintf("%04d-12-31", year)

	// Build hledger CLI args using options similar to other views
	opts := hledger.NewOptions().
		WithStartDate(start).
		WithEndDate(end).
		WithPeriod(hledger.PeriodWeekly).
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
	weekStart := 1
	weekEnd := len(header)
	last := strings.ToLower(strings.TrimSpace(header[len(header)-1]))
	if strings.Contains(last, "total") {
		weekEnd--
	}

	labels := make([]string, weekEnd-weekStart)
	for i := weekStart; i < weekEnd; i++ {
		labels[i-weekStart] = header[i]
	}

	vals := make([]float64, weekEnd-weekStart)
	for _, r := range rows[1:] {
		if len(r) < weekEnd { continue }
		if strings.EqualFold(strings.TrimSpace(r[0]), "total") { continue }
		for i := weekStart; i < weekEnd; i++ {
			v := parseAmount(r[i])
			if v < 0 { v = -v }
			vals[i-weekStart] += v
		}
	}
return labels, vals, nil
}

// getMonthlyBalances returns monthly absolute balances for the given account over the specified year.
func (t *TableGraph) getMonthlyBalances(year int, account string) ([]string, []float64, error) {
	return t.getMonthlyBalancesRange(year, account, fmt.Sprintf("%04d-12-31", year))
}

// getMonthlyBalancesRange returns monthly absolute balances for the given account over the specified year,
// ending at the provided endDate (YYYY-MM-DD), inclusive.
func (t *TableGraph) getMonthlyBalancesRange(year int, account string, endDate string) ([]string, []float64, error) {
	start := fmt.Sprintf("%04d-01-01", year)
	end := endDate
	if strings.TrimSpace(end) == "" { end = fmt.Sprintf("%04d-12-31", year) }

	opts := hledger.NewOptions().
		WithStartDate(start).
		WithEndDate(end).
		WithPeriod(hledger.PeriodMonthly).
		WithPretty(true).
		WithLayout(hledger.LayoutBare).
		WithOutputCSV(true)

	args := []string{"hledger", "balance", account}
	args = append(args, opts.Build()...)
	if lf := os.Getenv("HLEDGER_FILE"); strings.TrimSpace(lf) != "" { args = append(args, "-f", lf) }

	cmd := exec.Command(args[0], args[1:]...)
	out, err := cmd.CombinedOutput()
	if err != nil { return nil, nil, fmt.Errorf("%v: %s", err, string(out)) }

	rows, err := parseCSV(bytes.NewReader(out))
	if err != nil { return nil, nil, fmt.Errorf("parse csv: %w", err) }
	if len(rows) < 2 { return nil, nil, fmt.Errorf("no data for %s", account) }

	header := rows[0]
	if len(header) < 2 { return nil, nil, fmt.Errorf("unexpected header for %s", account) }
	monthStart := 1
	monthEnd := len(header)
	last := strings.ToLower(strings.TrimSpace(header[len(header)-1]))
	if strings.Contains(last, "total") { monthEnd-- }

	months := make([]string, 0, monthEnd-monthStart)
	for i := 1; i <= (monthEnd-monthStart); i++ { months = append(months, time.Date(year, time.Month(i), 1, 0, 0, 0, 0, time.UTC).Format("Jan")) }

	vals := make([]float64, monthEnd-monthStart)
	for _, r := range rows[1:] {
		if len(r) < monthEnd { continue }
		if strings.EqualFold(strings.TrimSpace(r[0]), "total") { continue }
		for i := monthStart; i < monthEnd; i++ {
			v := parseAmount(r[i])
			if v < 0 { v = -v }
			vals[i-monthStart] += v
		}
	}
	return months, vals, nil
}


func strSliceToNumbers(s []string) []float64 {
	var numbers []float64
	for _, v := range s {
		n, err := strconv.ParseFloat(v, 64)
		if err != nil {
			continue
		}
		numbers = append(numbers, n)
	}
	return numbers
}

// getLedgerLastDate returns the date (YYYY-MM-DD) of the last transaction in the ledger according to hledger print -O csv
func getLedgerLastDate() (string, error) {
	args := []string{"hledger", "print", "-O", "csv"}
	if lf := os.Getenv("HLEDGER_FILE"); strings.TrimSpace(lf) != "" { args = append(args, "-f", lf) }
	cmd := exec.Command(args[0], args[1:]...)
	out, err := cmd.CombinedOutput()
	if err != nil {
		return "", fmt.Errorf("%v: %s", err, string(out))
	}
	rows, err := parseCSV(bytes.NewReader(out))
	if err != nil {
		return "", fmt.Errorf("parse csv: %w", err)
	}
	if len(rows) < 2 {
		return "", fmt.Errorf("no transactions")
	}
	// Last non-empty date in first column
	for i := len(rows) - 1; i >= 1; i-- {
		if len(rows[i]) > 0 {
			d := strings.TrimSpace(rows[i][0])
			if d != "" {
				return d, nil
			}
		}
	}
	return "", fmt.Errorf("no dates found")
}

func (t *TableGraph) setContentGraph() {
	// Delegate to unified updater so behavior stays consistent
	t.updateGraph()
}
