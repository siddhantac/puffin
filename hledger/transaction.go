package hledger

import (
	"encoding/csv"
	"errors"
	"fmt"
	"io"
	"strings"
)

type Transaction struct {
	ID               string
	Date             string
	Account          string
	AccountShortName string
	Description      string
	Amount           string
}

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
			Account:     shortAccountName(record[7]),
			Amount:      record[8],
		}

		txns = append(txns, txn)
	}

	return txns
}

func parseCSVForReg(data io.Reader) []Transaction {
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
			ID:               record[0],
			Date:             record[1],
			Description:      record[3],
			Account:          record[4],
			AccountShortName: shortAccountName(record[4]),
			Amount:           record[5],
		}

		txns = append(txns, txn)
	}

	return txns
}

func shortAccountName(s string) string {
	s = strings.Replace(s, "liabilities", "lia", 1)
	s = strings.Replace(s, "expenses", "exp", 1)
	s = strings.Replace(s, "credit_card", "cc", 1)
	s = strings.Replace(s, "income", "inc", 1)
	s = strings.Replace(s, "assets", "ast", 1)
	return s
}
