package hledger

import (
	"encoding/csv"
	"errors"
	"fmt"
	"io"
)

func parseCSV(data io.Reader, skipRows int) ([][]string, error) {
	result := make([][]string, 0)

	csvReader := csv.NewReader(data)

	for i := 0; i < skipRows; i++ {
		_, _ = csvReader.Read()
	}

	for {
		record, err := csvReader.Read()
		if errors.Is(err, io.EOF) {
			break
		}

		if err != nil {
			return nil, fmt.Errorf("failed to read csv: %w", err)
		}

		// TODO: handle shortAccountNames
		result = append(result, record)
	}

	return result, nil
}
