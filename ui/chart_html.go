package ui

import (
	"bytes"
	"fmt"
	"strings"
)

// buildRevenueChartHTML generates a minimal HTML page embedding tui.chart
// and rendering a stacked column chart for the given revenue data.
// It uses CDN links for toastui-chart CSS and JS.
func buildRevenueChartHTML(rd *revenueData, width, height int) string {
	// Categories are months; if not 12, just use provided labels
	cats := make([]string, 0, len(rd.months))
	for _, m := range rd.months {
		cats = append(cats, fmt.Sprintf("\"%s\"", m))
	}

	// Series arrays: Top 1..3 and other
	arr := func(vals []float64) string {
		var b bytes.Buffer
		for i, v := range vals {
			if i > 0 {
				b.WriteString(",")
			}
			b.WriteString(fmt.Sprintf("%g", v))
		}
		return b.String()
	}

	js := fmt.Sprintf(`
<!doctype html>
<html>
<head>
  <meta charset="utf-8">
  <title>revenue</title>
  <meta name="viewport" content="width=device-width,initial-scale=1">
  <link rel="stylesheet" href="https://uicdn.toast.com/chart/latest/toastui-chart.min.css" />
  <style>
    html,body{margin:0;padding:0;background:#111;color:#ddd}
    #chart{width:%dpx;height:%dpx}
  </style>
</head>
<body>
  <div id="chart"></div>
  <script src="https://uicdn.toast.com/chart/latest/toastui-chart.min.js"></script>
  <script>
    const el = document.getElementById('chart');
    const data = {
      categories: [%s],
      series: [
        { name: 'Top 1', data: [%s] },
        { name: 'Top 2', data: [%s] },
        { name: 'Top 3', data: [%s] },
        { name: 'other', data: [%s] }
      ]
    };
    const options = {
      chart: { width: %d, height: %d, title: 'Revenue by Source (stacked) %d' },
      series: { stack: true },
      legend: { align: 'left' },
      theme: {
        series: { colors: ['#FFA500','#1E90FF','#32CD32','#808080'] }
      },
      yAxis: { title: 'Dollars' },
      xAxis: { title: 'Month' }
    };
    const chart = toastui.Chart.columnChart({ el, data, options });
    setTimeout(() => { document.title = 'READY'; }, 500);
  </script>
</body>
</html>
`,
		width, height,
		strings.Join(cats, ","),
		arr(rd.top3[0]),
		arr(rd.top3[1]),
		arr(rd.top3[2]),
		arr(rd.other),
		width, height, rd.year,
	)
	return js
}

