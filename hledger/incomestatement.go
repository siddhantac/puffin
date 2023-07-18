package hledger

import (
	_ "embed"
	"encoding/csv"
	"errors"
	"fmt"
	"io"
)

type IncomeStatement struct {
	Name    string
	Amounts []string
}

func (h Hledger) IncomeStatement(filters ...Filter) ([][]string, error) {
	rd, err := execCmd("incomestatement", true, filters...)
	if err != nil {
		return nil, err
	}

	data := parseCSV(rd, 1)
	return data, nil
}

func parseCSVIncomeStatement(data io.Reader) []IncomeStatement {
	result := make([]IncomeStatement, 0)
	csvReader := csv.NewReader(data)
	_, _ = csvReader.Read() // ignore the first row

	for {
		record, err := csvReader.Read()
		if errors.Is(err, io.EOF) {
			break
		}

		if err != nil {
			fmt.Println("error:", err)
			break
		}

		res := IncomeStatement{
			Name:    record[0],
			Amounts: make([]string, 0, len(record[1:])),
		}

		for _, cols := range record[1:] {
			res.Amounts = append(res.Amounts, cols)
		}

		result = append(result, res)
	}

	return result
}
