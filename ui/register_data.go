package ui

import "github.com/charmbracelet/bubbles/table"

type registerData struct {
	Rows    []table.Row
	Columns table.Row
}

func createRegisterData(data [][]string) registerData {
	bd := registerData{
		Rows:    make([]table.Row, 0, len(data)-1),
		Columns: skipRegisterColumns(data[0]),
	}

	// reverse the slice since register returns
	// oldest transactions first
	for i := len(data) - 1; i >= 1; i-- {
		bd.Rows = append(bd.Rows, skipRegisterColumns(data[i]))
	}

	return bd
}

func skipRegisterColumns(row []string) []string {
	return []string{row[0], row[1], row[3], row[4], row[5]}
}
