package ui

import "github.com/charmbracelet/bubbles/table"

type registerData struct {
	Rows    []table.Row
	Columns table.Row
}

func createRegisterData(data [][]string) registerData {
	bd := registerData{
		Rows:    make([]table.Row, 0, len(data)),
		Columns: skipColumns(data[0]),
	}

	for _, d := range data[1:] {
		bd.Rows = append(bd.Rows, skipColumns(d))
	}

	return bd
}

func skipColumns(row []string) []string {
	return []string{row[0], row[1], row[3], row[4], row[5]}
}
