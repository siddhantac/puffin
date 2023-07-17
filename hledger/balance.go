package hledger

import (
	"encoding/csv"
	"errors"
	"fmt"
	"io"
)

func (h Hledger) Balance(filters ...Filter) ([]Account, error) {
	rd, err := execCmd("balance", true, filters...)
	if err != nil {
		return nil, err
	}

	data := parseCSVBalance(rd)
	return data, nil
}

type Account struct {
	Name   string
	Amount string
}

func parseCSVBalance(data io.Reader) []Account {
	accs := make([]Account, 0)

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

		acc := Account{
			Name:   shortAccountName(record[0]),
			Amount: record[1],
		}

		accs = append(accs, acc)
	}

	return accs
}
