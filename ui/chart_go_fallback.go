package ui

import (
	"bytes"
	"fmt"
	"os"
	"path/filepath"
	"strings"
	"time"

	chart "github.com/wcharczuk/go-chart/v2"
	"github.com/wcharczuk/go-chart/v2/drawing"
)

// renderGoLineChartPNG renders a multi-series line chart to a PNG using go-chart and returns the PNG file path.
// If yMax <= yMin, the Y-axis auto-scales. If step > 0 and yMax > yMin, fixed ticks are used.
func renderGoLineChartPNG(names []string, series [][]float64, categories []string, width, height int, title, xAxisTitle string, yMin, yMax, step float64) (string, error) {
	if width <= 0 { width = 1100 }
	if height <= 0 { height = 560 }
	// X axis uses index positions 0..n-1
	n := 0
	for _, s := range series { if len(s) > n { n = len(s) } }
	if n == 0 { return "", fmt.Errorf("no series data") }
	xs := make([]float64, n)
	for i := 0; i < n; i++ { xs[i] = float64(i) }

	// Build X ticks from non-empty category labels
	xTicks := []chart.Tick{}
	for i := 0; i < len(categories) && i < n; i++ {
		lab := strings.TrimSpace(categories[i])
		if lab == "" { continue }
		xTicks = append(xTicks, chart.Tick{Value: float64(i), Label: lab})
	}

	// Build series
	all := []chart.Series{}
	palette := []drawing.Color{
		chart.GetDefaultColor(0),
		chart.GetDefaultColor(1),
		chart.GetDefaultColor(2),
		chart.GetDefaultColor(3),
	}
	for si, s := range series {
		ys := make([]float64, n)
		copy(ys, s)
		// pad if shorter
		if len(ys) < n {
			pad := make([]float64, n-len(ys))
			ys = append(ys, pad...)
		}
		cs := chart.ContinuousSeries{
			Name:    nameOrIndex(names, si),
			XValues: xs,
			YValues: ys,
		}
		// assign colors
		cs.Style = chart.Style{
			StrokeColor: palette[si%len(palette)],
			StrokeWidth: 2.0,
		}
		all = append(all, cs)
	}

	// Y axis config
	var yRange *chart.ContinuousRange
	var yTicks []chart.Tick
	if yMax > yMin {
		yRange = &chart.ContinuousRange{Min: yMin, Max: yMax}
		if step > 0 {
			for v := yMin; v <= yMax+1e-6; v += step {
				yTicks = append(yTicks, chart.Tick{Value: v, Label: fmt.Sprintf("%.0f", v)})
			}
		}
	}

	// Assemble chart
	c := chart.Chart{
		Title: title,
		Width:  width,
		Height: height,
		Background: chart.Style{Padding: chart.NewBox(20, 20, 20, 40)},
		XAxis: chart.XAxis{
			Name:  xAxisTitle,
			Ticks: xTicks,
		},
		YAxis: chart.YAxis{
			Range: yRange,
			Ticks: yTicks,
		},
		Series: all,
	}

	// Render to buffer
	buf := bytes.NewBuffer(nil)
	if err := c.Render(chart.PNG, buf); err != nil { return "", err }

	// Write to temp file
	file := filepath.Join(os.TempDir(), fmt.Sprintf("puffin_go_chart_%d.png", time.Now().UnixNano()))
	if err := os.WriteFile(file, buf.Bytes(), 0o644); err != nil { return "", err }
	return file, nil
}

func nameOrIndex(names []string, i int) string {
	if i >= 0 && i < len(names) && strings.TrimSpace(names[i]) != "" { return names[i] }
	return fmt.Sprintf("Series %d", i+1)
}

