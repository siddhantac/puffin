package hledger

import (
	"encoding/csv"
	"errors"
	"fmt"
	"io"
)

func (h Hledger) Balance(filters ...Filter) ([][]string, error) {
	rd, err := execCmd("balance", true, filters...)
	if err != nil {
		return nil, err
	}

	data := parseCSVBalance(rd)
	return data, nil
}

func parseCSVBalance(data io.Reader) [][]string {
	result := make([][]string, 0)

	csvReader := csv.NewReader(data)

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
