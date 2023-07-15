package hledger

import (
	"encoding/csv"
	"errors"
	"fmt"
	"io"
)

func parseCSVForPrint(data io.Reader) []Transaction {
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

		txn := Transaction{
			ID:          record[0],
			Date:        record[1],
			Description: record[5],
			FromAccount: shortAccountName(record[7]),
			Amount:      record[8],
		}

		txns = append(txns, txn)
	}

	return txns
}
