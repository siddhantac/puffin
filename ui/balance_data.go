package ui

import (
	"github.com/charmbracelet/bubbles/table"
)

type balanceData struct {
	Rows    []table.Row
	Columns table.Row
}

func createBalanceData(data [][]string) balanceData {
	bd := balanceData{
		Rows:    make([]table.Row, 0, len(data)),
		Columns: data[0],
	}

	for _, d := range data[1:] {
		bd.Rows = append(bd.Rows, d)
	}

	return bd
}

type revenueData struct {
	Rows    []table.Row
	Columns table.Row
}

func createRevenueData(data [][]string) revenueData {
	ad := revenueData{
		Rows:    make([]table.Row, 0, len(data)),
		Columns: data[0],
	}

	for _, d := range data[1:] {
		ad.Rows = append(ad.Rows, d)
	}

	return ad
}

type liabilitiesData struct {
	Rows    []table.Row
	Columns table.Row
}

func createLiabilitiesData(data [][]string) liabilitiesData {
	ad := liabilitiesData{
		Rows:    make([]table.Row, 0, len(data)),
		Columns: data[0],
	}

	for _, d := range data[1:] {
		ad.Rows = append(ad.Rows, d)
	}

	return ad
}
