package report

import (
	"fmt"
	"puffin/ledger"
	"time"
)

func NewMonthlyReport(transactions ledger.Transactions) error {
	before, err := time.Parse("2006-01-02", "2024-01-01")
	if err != nil {
		return err
	}

	after, err := time.Parse("2006-01-02", "2023-01-01")
	if err != nil {
		return err
	}

	transactions, err = transactions.FilterByDate(before, after)
	if err != nil {
		return err
	}

	if len(transactions) <= 0 {
		return fmt.Errorf("no transactions in given time period")
	}

	depth := 1
	balance := transactions.Balance(depth)
	for _, b := range balance {
		fmt.Println(b)
	}

	return nil
}
