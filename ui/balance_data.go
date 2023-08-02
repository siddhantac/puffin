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

type assetsData struct {
	Rows    []table.Row
	Columns table.Row
}

func createAssetsData(data [][]string) assetsData {
	ad := assetsData{
		Rows:    make([]table.Row, 0, len(data)),
		Columns: data[0],
	}

	for _, d := range data[1:] {
		ad.Rows = append(ad.Rows, d)
	}

	return ad
}

type expensesData struct {
	Rows    []table.Row
	Columns table.Row
}

func createExpensesData(data [][]string) expensesData {
	ad := expensesData{
		Rows:    make([]table.Row, 0, len(data)),
		Columns: data[0],
	}

	for _, d := range data[1:] {
		ad.Rows = append(ad.Rows, d)
	}

	return ad
}
