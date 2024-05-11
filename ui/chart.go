package ui

import (
	"github.com/NimbleMarkets/ntcharts/barchart"
	"github.com/charmbracelet/lipgloss"
)

func chartContent() string {
	d1 := barchart.BarData{
		Label: "A",
		Values: []barchart.BarValue{
			{
				Name:  "Item1",
				Value: 21.2,
				Style: lipgloss.NewStyle().Foreground(lipgloss.Color("10")),
			}}, // green
	}
	d2 := barchart.BarData{
		Label: "B",
		Values: []barchart.BarValue{
			{
				Name:  "Item1",
				Value: 15.2,
				Style: lipgloss.NewStyle().Foreground(lipgloss.Color("9")),
			}}, // red
	}

	bc := barchart.New(11, 10)
	bc.PushAll([]barchart.BarData{d1, d2})
	bc.Draw()

	return bc.View()
}
