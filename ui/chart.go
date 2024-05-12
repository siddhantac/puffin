package ui

import (
	"puffin/accounting"

	"github.com/NimbleMarkets/ntcharts/barchart"
	"github.com/charmbracelet/lipgloss"
)

func chartContent(values accounting.IncomeStatementChartData) string {
	d1 := barchart.BarData{
		Label: "Revenue",
		Values: []barchart.BarValue{
			{
				Name:  "Revenue",
				Value: values.Revenue,
				Style: lipgloss.NewStyle().Foreground(lipgloss.Color("10")),
			}}, // green
	}
	d2 := barchart.BarData{
		Label: "Expenses",
		Values: []barchart.BarValue{
			{
				Name:  "Expenses",
				Value: values.Expenses,
				Style: lipgloss.NewStyle().Foreground(lipgloss.Color("9")),
			}}, // red
	}

	bc := barchart.New(11, 10)
	bc.PushAll([]barchart.BarData{d1, d2})
	bc.Draw()

	return bc.View()
}
