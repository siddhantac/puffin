package hledger

import (
	_ "embed"
	"encoding/csv"
	"errors"
	"fmt"
	"io"
	"strings"
)

//go:embed incomestmt_y2.csv
var incomestmt1 string

//go:embed incomestmt2.csv
var incomestmt2 string

type IncomeStatement struct {
	Name    string
	Amounts []string
}

func (h Hledger) IncomeStatement1(filters ...Filter) ([]IncomeStatement, error) {
	return parseIncomeStatement(incomestmt1), nil
}

func (h Hledger) IncomeStatement2(filters ...Filter) ([]IncomeStatement, error) {
	return parseIncomeStatement(incomestmt2), nil
}

func (h Hledger) IncomeStatement(filters ...Filter) ([]IncomeStatement, error) {
	return nil, nil
}

func parseIncomeStatement(data string) []IncomeStatement {
	result := make([]IncomeStatement, 0)
	csvReader := csv.NewReader(strings.NewReader(data))
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
