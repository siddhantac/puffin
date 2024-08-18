package ui

import (
	"encoding/csv"
	"errors"
	"io"

	"github.com/charmbracelet/bubbles/table"
)

func parseCSV(r io.Reader) ([]table.Row, error) {
	result := make([]table.Row, 0)
	csvrdr := csv.NewReader(r)
	// csvrdr.Read() // skip 1 line
	for {
		rec, err := csvrdr.Read()
		if errors.Is(err, io.EOF) {
			break
		}
		if err != nil {
			return nil, err
		}
		result = append(result, rec)
	}
	return result, nil
}
