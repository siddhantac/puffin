package ui

import (
	"bytes"
	"context"
	"encoding/base64"
	"encoding/csv"
	"fmt"
	"math"
	"os"
	"os/exec"
	"path/filepath"
	"strconv"
	"strings"
	"time"

	"github.com/chromedp/chromedp"
	"github.com/charmbracelet/lipgloss"
	"github.com/go-echarts/go-echarts/v2/charts"
	"github.com/go-echarts/go-echarts/v2/components"
	"github.com/go-echarts/go-echarts/v2/opts"
)

// supportsInlineImages returns true for terminals likely to support inline images
// Add explicit overrides via PUFFIN_INLINE_IMAGES and whitelist Warp by TERM_PROGRAM
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
	if strings.Contains(tp, "warp") { // e.g., WarpTerminal
		return true
	}
	// Kitty protocol
	if strings.Contains(term, "kitty") {
		return true
	}
	return false
}

// renderInlineImage returns an iTerm2/Kitty inline-image escape sequence to display the PNG.
// If the terminal doesn't support it, returns an empty string.
func renderInlineImage(pngPath string) string {
	data, err := os.ReadFile(pngPath)
	if err != nil {
		return ""
	}
	b64 := base64.StdEncoding.EncodeToString(data)
	seq := fmt.Sprintf("\x1b]1337;File=size=%d;inline=1:%s\x07", len(data), b64)
	return seq
}

// bsSeries holds a single series of monthly values
type bsSeries struct {
	name string
	vals []float64
	col  string // hex color, optional
}

// bsData aggregates the asset and liability series for 2025
type bsData struct {
	months []string
	assets []bsSeries // two series
	liabs  []bsSeries // two series
}

// buildBSData fetches monthly balances for target accounts for the given year.
func buildBSData(year int) (*bsData, error) {
	assetAccounts := []string{"assets:bank:09", "assets:bank:chase"}
	liabAccounts := []string{"liabilities:0335", "liabilities:6154"}

	months, aset, err := fetchMonthlySeries(year, assetAccounts)
	if err != nil { return nil, err }
	_, liab, err := fetchMonthlySeries(year, liabAccounts)
	if err != nil { return nil, err }

	// assign default colors
	if len(aset) == 2 {
		aset[0].col = "#1E90FF" // blue
		aset[1].col = "#FFA500" // orange
	}
	if len(liab) == 2 {
		liab[0].col = "#FF00FF" // magenta
		liab[1].col = "#00CED1" // dark turquoise
	}

	return &bsData{months: months, assets: aset, liabs: liab}, nil
}

// fetchMonthlySeries returns months and per-account absolute monthly balances
func fetchMonthlySeries(year int, accounts []string) ([]string, []bsSeries, error) {
	var months []string
	series := make([]bsSeries, 0, len(accounts))
	for _, acct := range accounts {
		m, vals, err := getMonthlyBalances(year, acct)
		if err != nil { return nil, nil, err }
		if len(months) == 0 { months = m }
		name := acct
		if idx := strings.LastIndex(acct, ":"); idx != -1 && idx+1 < len(acct) { name = acct[idx+1:] }
		series = append(series, bsSeries{name: name, vals: vals})
	}
	return months, series, nil
}

// getMonthlyBalances runs hledger balance for a single account over the year, returning month labels and abs values
func getMonthlyBalances(year int, account string) ([]string, []float64, error) {
	start := fmt.Sprintf("%04d-01-01", year)
	end := fmt.Sprintf("%04d-12-31", year)
	args := []string{"hledger", "balance", account, "--monthly", "--pretty", "-O", "csv", "--layout", "bare", "-b", start, "-e", end}
	// Respect HLEDGER_FILE if set
	if lf := os.Getenv("HLEDGER_FILE"); strings.TrimSpace(lf) != "" { args = append(args, "-f", lf) }

	cmd := exec.Command(args[0], args[1:]...)
	out, err := cmd.CombinedOutput()
	if err != nil {
		return nil, nil, fmt.Errorf("%v: %s", err, string(out))
	}

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
	for i := 1; i <= (monthEnd-monthStart); i++ {
		months = append(months, time.Date(year, time.Month(i), 1, 0, 0, 0, 0, time.UTC).Format("Jan"))
	}

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

// parseCSV is a local lightweight CSV reader (avoid cross-package dependency)
func parseCSV(r *bytes.Reader) ([][]string, error) {
	res := make([][]string, 0)
	cr := csv.NewReader(r)
	for {
		rec, err := cr.Read()
		if err != nil {
			if strings.Contains(err.Error(), "EOF") { break }
			return nil, err
		}
		res = append(res, rec)
	}
	return res, nil
}

func parseAmount(s string) float64 {
	s = strings.TrimSpace(s)
	if s == "" { return 0 }
	neg := false
	if strings.HasPrefix(s, "(") && strings.HasSuffix(s, ")") {
		neg = true
		s = strings.TrimPrefix(strings.TrimSuffix(s, ")"), "(")
	}
	replacer := strings.NewReplacer(",", "", "$", "", "€", "", "£", "", "USD", "", "EUR", "", "%", "")
	s = replacer.Replace(s)
	s = strings.TrimSpace(s)
	if s == "" || s == "-" { return 0 }
	v, err := strconv.ParseFloat(s, 64)
	if err != nil { return 0 }
	if neg { v = -v }
	return v
}

// buildBSChartsHTML uses go-echarts to build two stacked line charts for assets and liabilities
func buildBSChartsHTML(data *bsData, width, height int) (string, error) {
	if data == nil || len(data.months) == 0 { return "", fmt.Errorf("no data") }

	assetsLine := charts.NewLine()
assetsLine.SetGlobalOptions(
		charts.WithTitleOpts(opts.Title{Title: "Assets — 2025"}),
		charts.WithYAxisOpts(opts.YAxis{
			AxisLabel:   &opts.AxisLabel{Formatter: "{value}"},
			SplitNumber: 5,
Scale:       opts.Bool(true),
		}),
	)
	assetsLine.SetXAxis(data.months)
	assetsLine.SetGlobalOptions(
		charts.WithInitializationOpts(opts.Initialization{
			// Make chart fill its container. We'll size the containers via CSS.
			Width:  "100%",
			Height: "360px",
		}),
	)
	for _, s := range data.assets {
		items := make([]opts.LineData, len(s.vals))
		for i, v := range s.vals { items[i] = opts.LineData{Value: v} }
		assetsLine.AddSeries(s.name, items)
	}

	liabsLine := charts.NewLine()
liabsLine.SetGlobalOptions(
		charts.WithTitleOpts(opts.Title{Title: "Liabilities — 2025"}),
		charts.WithYAxisOpts(opts.YAxis{
			AxisLabel:   &opts.AxisLabel{Formatter: "{value}"},
			SplitNumber: 5,
Scale:       opts.Bool(true),
		}),
	)
	liabsLine.SetXAxis(data.months)
	liabsLine.SetGlobalOptions(
		charts.WithInitializationOpts(opts.Initialization{
			// Make chart fill its container. We'll size the containers via CSS.
			Width:  "100%",
			Height: "360px",
		}),
	)
	for _, s := range data.liabs {
		items := make([]opts.LineData, len(s.vals))
		for i, v := range s.vals { items[i] = opts.LineData{Value: v} }
		liabsLine.AddSeries(s.name, items)
	}

	page := components.NewPage()
	page.AddCharts(assetsLine, liabsLine)

	var buf bytes.Buffer
	if err := page.Render(&buf); err != nil { return "", err }

	// Wrap into a minimal html with a container id; wait on and CSS to force side-by-side charts
	html := fmt.Sprintf(`<!DOCTYPE html>
<html>
<head>
<meta charset="utf-8">
<title>B/S Charts</title>
<style>
/* Force side-by-side layout for go-echarts chart containers */
#page { white-space: nowrap; }
#page .chart-container { display: inline-block !important; vertical-align: top; width: 50% !important; height: 360px !important; box-sizing: border-box; }
</style>
</head>
<body>
<div id="page">%s</div>
</body>
</html>`, buf.String())
	return html, nil
}

// renderBSChartsPNG screenshots the generated HTML into a PNG via headless Chrome
func renderBSChartsPNG(data *bsData, width, height int) (string, error) {
	html, err := buildBSChartsHTML(data, width, height)
	if err != nil { return "", err }

	ctx, cancel := chromedp.NewContext(context.Background())
	defer cancel()
	ctx, cancel = context.WithTimeout(ctx, 20*time.Second)
	defer cancel()

	var pngBuf []byte
	url := "data:text/html;base64," + base64.StdEncoding.EncodeToString([]byte(html))
	tasks := chromedp.Tasks{
		chromedp.Navigate(url),
		chromedp.WaitReady("#page"),
		chromedp.Sleep(800 * time.Millisecond),
		chromedp.FullScreenshot(&pngBuf, 100),
	}
	if err := chromedp.Run(ctx, tasks); err != nil { return "", err }

	dir := os.TempDir()
	file := filepath.Join(dir, fmt.Sprintf("puffin_bs_%d.png", time.Now().UnixNano()))
	if err := os.WriteFile(file, pngBuf, 0o644); err != nil { return "", err }
	return file, nil
}

// ascii fallback: two simple line charts side-by-side
func renderBSChartsASCII(data *bsData, width, height int) string {
	if width < 60 { width = 60 }
	if height < 12 { height = 12 }
	// Half width per chart
	w := max(30, width/2)
	assets := renderMultiLineASCII("Assets — 2025", data.months, data.assets, w, height)
	liabs := renderMultiLineASCII("Liabilities — 2025", data.months, data.liabs, w, height)
	return lipgloss.JoinHorizontal(lipgloss.Top, assets, liabs)
}

func renderMultiLineASCII(title string, months []string, series []bsSeries, width, height int) string {
	if len(months) == 0 || len(series) == 0 { return "No data" }
	if width < 12 { width = 12 }
	if height < 6 { height = 6 }
	// y-range
	var yMax float64
	for _, s := range series { for _, v := range s.vals { if v > yMax { yMax = v } } }
	if yMax == 0 { yMax = 1 }
	grid := make([][]rune, height)
	for i := range grid { grid[i] = make([]rune, width); for j := range grid[i] { grid[i][j] = ' ' } }
	n := len(months)
	xpos := make([]int, n)
	for i := 0; i < n; i++ { xpos[i] = int(math.Round(float64(i) / float64(max(1, n-1)) * float64(width-1))) }
	markers := []rune{'●','×','•','■'}
	for si, s := range series {
		mk := markers[si%len(markers)]
		for i := 0; i < n; i++ {
			v := s.vals[i]
			y := int(math.Round((1.0 - (v/yMax)) * float64(height-1)))
			if y < 0 { y = 0 }
			if y >= height { y = height-1 }
			grid[y][xpos[i]] = mk
		}
	}
	lines := make([]string, height)
	for i := 0; i < height; i++ { lines[i] = string(grid[i]) }
	// labels row
	var labelRow strings.Builder
	for i := 0; i < n; i++ {
		label := months[i]
		if len(label) > 3 { label = label[:3] }
		pos := xpos[i]
		for labelRow.Len() < pos { labelRow.WriteByte(' ') }
		labelRow.WriteString(label)
	}
	titleStyle := lipgloss.NewStyle().Bold(true)
	graphStyle := lipgloss.NewStyle().Border(lipgloss.NormalBorder()).BorderForeground(lipgloss.Color("240")).Padding(0)
	legend := renderLegendASCII(series)

	// Build y-axis labels (left side) with up to 5 ticks
	tickCount := 5
	if height < tickCount { tickCount = height }
	labelForRow := make(map[int]string)
	maxLabelLen := 0
	for t := 0; t <= tickCount; t++ {
		row := int(math.Round(float64(t) / float64(max(1, tickCount)) * float64(height-1)))
		val := yMax * (1.0 - float64(row)/float64(height-1))
		lab := formatTick(val)
		labelForRow[row] = lab
		if l := len(lab); l > maxLabelLen { maxLabelLen = l }
	}
	labelWidth := max(4, maxLabelLen) // at least 4 chars wide

	// Render the graph with a border
	bordered := graphStyle.Render(strings.Join(lines, "\n"))
	borderLines := strings.Split(bordered, "\n")
	finalLines := make([]string, 0, len(borderLines))
	for i, bl := range borderLines {
		if i == 0 || i == len(borderLines)-1 {
			// top/bottom border: just pad
			finalLines = append(finalLines, strings.Repeat(" ", labelWidth+3)+bl)
			continue
		}
		row := i - 1 // map to grid row
		if lab, ok := labelForRow[row]; ok {
			finalLines = append(finalLines, fmt.Sprintf("%*s ┤ %s", labelWidth, lab, bl))
		} else {
			finalLines = append(finalLines, fmt.Sprintf("%*s │ %s", labelWidth, "", bl))
		}
	}

	// Indent x-axis labels to align under the chart area
	xIndent := strings.Repeat(" ", labelWidth+3)

	return lipgloss.JoinVertical(
		lipgloss.Left,
		titleStyle.Render(title),
		legend,
		strings.Join(finalLines, "\n"),
		xIndent+labelRow.String(),
	)
}

func renderLegendASCII(series []bsSeries) string {
	items := make([]string, 0, len(series))
	for _, s := range series {
		items = append(items, fmt.Sprintf("■ %s", s.name))
	}
	return lipgloss.JoinHorizontal(lipgloss.Left, lipgloss.NewStyle().Bold(true).Render("KEY"), "  ", strings.Join(items, "   "))
}

// formatTick returns a compact human-readable label for large numbers
func formatTick(v float64) string {
	av := math.Abs(v)
	switch {
	case av >= 1e12:
		return fmt.Sprintf("%.1fT", v/1e12)
	case av >= 1e9:
		return fmt.Sprintf("%.1fB", v/1e9)
	case av >= 1e6:
		return fmt.Sprintf("%.1fM", v/1e6)
	case av >= 1e3:
		return fmt.Sprintf("%.1fk", v/1e3)
	default:
		return fmt.Sprintf("%.0f", v)
	}
}

// GenerateBSGraphs returns either an inline image escape sequence or ASCII fallback
func GenerateBSGraphs(year int, width, height int) (string, error) {
	data, err := buildBSData(year)
	if err != nil { return "", err }
	if supportsInlineImages() {
		png, err := renderBSChartsPNG(data, width, height)
		if err == nil && png != "" {
			if img := renderInlineImage(png); img != "" { return img, nil }
		}
		// fallthrough to ascii if anything fails
	}
	return renderBSChartsASCII(data, width, height), nil
}

// max helper
func max(a, b int) int { if a > b { return a }; return b }

