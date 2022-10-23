package hledger

import (
	"encoding/csv"
	"errors"
	"fmt"
	"io"
)

type Transaction struct {
	ID          string
	Date        string
	FromAccount string
	ToAccount   string
	Description string
	Amount      string
}

func parseFromCSV(data io.Reader) []Transaction {
	txns := make([]Transaction, 0)

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

		modRecord := []string{}
		modRecord = append(modRecord, record[0], record[1], record[5], record[7], record[8])
		txn := Transaction{
			ID:          record[0],
			Date:        record[1],
			Description: record[5],
			FromAccount: record[7],
			Amount:      record[8],
		}

		txns = append(txns, txn)
	}

	return txns
}
