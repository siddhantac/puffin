package ui

import (
	"fmt"
	"math"
	"strings"
)

// buildLineChartHTML generates a self-contained HTML page with an inline SVG multi-series line chart.
// No external JS/CSS is required (offline, CDN-free). Auto-scales Y.
func buildLineChartHTML(names []string, series [][]float64, categories []string, width, height int, title string, xAxisTitle string) string {
	return buildLineChartHTMLWithScale(names, series, categories, width, height, title, xAxisTitle, 0, 0, 0)
}

// buildLineChartHTMLWithScale is like buildLineChartHTML but allows explicit Y-axis min/max and tick step.
// If yMax <= yMin or step <= 0, it auto-scales using data max and 5 ticks.
func buildLineChartHTMLWithScale(names []string, series [][]float64, categories []string, width, height int, title string, xAxisTitle string, yMin, yMax, step float64) string {
	if width <= 0 { width = 800 }
	if height <= 0 { height = 400 }
	// Layout
	marginTop, marginRight, marginBottom, marginLeft := 30, 20, 40, 60
	plotW := width - marginLeft - marginRight
	plotH := height - marginTop - marginBottom
	if plotW < 100 { plotW = 100 }
	if plotH < 100 { plotH = 100 }

	// Data bounds
	if !(yMax > yMin) {
		for _, s := range series {
			for _, v := range s {
				if v > yMax { yMax = v }
			}
		}
		if yMax == 0 { yMax = 1 }
		yMin = 0
	}
	yRange := yMax - yMin
	if yRange <= 0 { yRange = 1 }

	n := len(categories)
	// Safe positions
	xAt := func(i int) int {
		if n <= 1 { return marginLeft }
		return marginLeft + int(math.Round(float64(i)/float64(n-1)*float64(plotW)))
	}
	yAt := func(v float64) int {
		if v < yMin { v = yMin }
		if v > yMax { v = yMax }
		y := marginTop + plotH - int(math.Round(((v-yMin)/yRange)*float64(plotH)))
		if y < marginTop { y = marginTop }
		if y > marginTop+plotH { y = marginTop + plotH }
		return y
	}

	palette := []string{"#1E90FF", "#00CED1", "#32CD32", "#FFD700", "#FF00FF", "#FFA500", "#FF69B4"}

	// Build polylines
	var polylines []string
	for si, s := range series {
		color := palette[si%len(palette)]
		var pts []string
		for i := 0; i < n && i < len(s); i++ {
			pts = append(pts, fmt.Sprintf("%d,%d", xAt(i), yAt(s[i])))
		}
		polylines = append(polylines, fmt.Sprintf("<polyline fill=\"none\" stroke=\"%s\" stroke-width=\"2\" points=\"%s\" />", color, strings.Join(pts, " ")))
	}

	// Axes
	x1, y1 := marginLeft, marginTop+plotH
	x2, y2 := marginLeft+plotW, marginTop
	axes := fmt.Sprintf("<line x1=\"%d\" y1=\"%d\" x2=\"%d\" y2=\"%d\" stroke=\"#999\"/>\n<line x1=\"%d\" y1=\"%d\" x2=\"%d\" y2=\"%d\" stroke=\"#999\"/>", x1, y1, x1, y2, x1, y1, x2, y1)

	// X ticks: if categories contains <=12 non-empty labels, place them exactly at their indices;
	// otherwise sample to at most 12 labels evenly.
	maxLabels := 12
	var xTicks []string
	nonEmpty := 0
	for i := 0; i < n; i++ {
		if strings.TrimSpace(categories[i]) != "" { nonEmpty++ }
	}
	if nonEmpty > 0 && nonEmpty <= maxLabels {
		for i := 0; i < n; i++ {
			lab := strings.TrimSpace(categories[i])
			if lab == "" { continue }
			if len(lab) > 3 { lab = lab[:3] }
			x := xAt(i)
			y := y1 + 14
			xTicks = append(xTicks, fmt.Sprintf("<text x=\"%d\" y=\"%d\" fill=\"#ddd\" font-size=\"12\" text-anchor=\"middle\">%s</text>", x, y, lab))
		}
	} else {
		stride := 1
		if n > maxLabels { stride = int(math.Ceil(float64(n)/float64(maxLabels))) }
		for i := 0; i < n; i += stride {
			label := categories[i]
			if len(label) > 3 { label = label[:3] }
			x := xAt(i)
			y := y1 + 14
			xTicks = append(xTicks, fmt.Sprintf("<text x=\"%d\" y=\"%d\" fill=\"#ddd\" font-size=\"12\" text-anchor=\"middle\">%s</text>", x, y, label))
		}
	}
	// Y ticks (auto 5 or fixed step)
	var yTicks []string
	if step > 0 && yMax > yMin {
		for val := yMin; val <= yMax+1e-6; val += step {
			y := yAt(val)
			yTicks = append(yTicks, fmt.Sprintf("<text x=\"%d\" y=\"%d\" fill=\"#ddd\" font-size=\"12\" text-anchor=\"end\">%.0f</text>", marginLeft-6, y+4, val))
			// Optional grid
			yTicks = append(yTicks, fmt.Sprintf("<line x1=\"%d\" y1=\"%d\" x2=\"%d\" y2=\"%d\" stroke=\"#444\" stroke-dasharray=\"3,3\"/>", marginLeft, y, marginLeft+plotW, y))
		}
	} else {
		for t := 0; t <= 5; t++ {
			val := yMax * float64(t) / 5
			y := yAt(val)
			yTicks = append(yTicks, fmt.Sprintf("<text x=\"%d\" y=\"%d\" fill=\"#ddd\" font-size=\"12\" text-anchor=\"end\">%s</text>", marginLeft-6, y+4, formatTick(val)))
			// Optional grid
			yTicks = append(yTicks, fmt.Sprintf("<line x1=\"%d\" y1=\"%d\" x2=\"%d\" y2=\"%d\" stroke=\"#444\" stroke-dasharray=\"3,3\"/>", marginLeft, y, marginLeft+plotW, y))
		}
	}

	// Legend
	var legendItems []string
	lx, ly := marginLeft, marginTop-8
	for si, name := range names {
		color := palette[si%len(palette)]
		legendItems = append(legendItems, fmt.Sprintf("<rect x=\"%d\" y=\"%d\" width=\"10\" height=\"10\" fill=\"%s\" /> <text x=\"%d\" y=\"%d\" fill=\"#ddd\" font-size=\"12\">%s</text>", lx, ly-10, color, lx+14, ly, name))
		lx += 120
	}

	// Title and axis title
	titleSVG := fmt.Sprintf("<text x=\"%d\" y=\"%d\" fill=\"#ddd\" font-size=\"16\" font-weight=\"bold\">%s</text>", marginLeft, 20, title)
	xAxisTitleSVG := fmt.Sprintf("<text x=\"%d\" y=\"%d\" fill=\"#ddd\" font-size=\"12\">%s</text>", marginLeft+(plotW/2), height-6, xAxisTitle)

	svg := fmt.Sprintf(`<svg xmlns="http://www.w3.org/2000/svg" width="%d" height="%d" viewBox="0 0 %d %d">
  <rect x="0" y="0" width="%d" height="%d" fill="#111"/>
  %s
  %s
  %s
  %s
  %s
  %s
  %s
</svg>`, width, height, width, height, width, height, titleSVG, strings.Join(legendItems, "\n  "), axes, strings.Join(xTicks, "\n  "), strings.Join(yTicks, "\n  "), strings.Join(polylines, "\n  "), xAxisTitleSVG)

	html := fmt.Sprintf(`<!doctype html>
<html>
<head>
  <meta charset="utf-8">
  <title>chart</title>
  <meta name="viewport" content="width=device-width,initial-scale=1">
  <style>html,body{margin:0;padding:0;background:#111;color:#ddd;font-family:-apple-system,BlinkMacSystemFont,Segoe UI,Roboto,Helvetica,Arial,sans-serif}#chart{width:%dpx;height:%dpx}</style>
</head>
<body>
  <div id="chart">%s</div>
</body>
</html>`, width, height, svg)
	return html
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

