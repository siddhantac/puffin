package hledger

import (
	"encoding/csv"
	"errors"
	"fmt"
	"io"
)

type Account struct {
	Name   string
	Amount string
}

func parseAccountsFromCSV(data io.Reader) []Account {
	accs := make([]Account, 0)

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

		acc := Account{
			Name:   shortAccountName(record[0]),
			Amount: record[1],
		}

		accs = append(accs, acc)
	}

	return accs
}
