package hledger

import (
	"encoding/csv"
	"errors"
	"fmt"
	"io"
	"strings"
)

type Transaction struct {
	ID          string
	Date        string
	FromAccount string
	ToAccount   string
	Description string
	Amount      string
}

func parseTransactionsFromCSV(data io.Reader) []Transaction {
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

	size := len(txns)
	reversedTxns := make([]Transaction, size)
	for i := range txns {
		reversedTxns[i] = txns[size-1-i]
	}

	return reversedTxns
}

func shortAccountName(s string) string {
	s = strings.Replace(s, "liabilities", "lia", 1)
	s = strings.Replace(s, "expenses", "exp", 1)
	s = strings.Replace(s, "credit_card", "cc", 1)
	s = strings.Replace(s, "income", "inc", 1)
	s = strings.Replace(s, "assets", "ast", 1)
	return s
}
