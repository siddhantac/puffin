package hledger

import (
	"encoding/csv"
	"errors"
	"fmt"
	"io"
)

func parseCSV(data io.Reader, skipRows int) [][]string {
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
			fmt.Println("error:", err)
			break
		}

		// TODO: handle shortAccountNames
		result = append(result, record)
	}

	return result
}
