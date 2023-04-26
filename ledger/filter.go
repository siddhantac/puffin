package ledger

import (
	"fmt"
	"time"
)

func (t Transactions) FilterByDate(before, after time.Time) (Transactions, error) {
	if before.IsZero() && after.IsZero() {
		return t, nil
	}

	filteredTransactions := make(Transactions, 0)
	if !before.IsZero() && !after.IsZero() {
		for _, txn := range t {
			if txn.Date.Before(before) && txn.Date.After(after) {
				filteredTransactions = append(filteredTransactions, txn)
			}
		}

		return filteredTransactions, nil
	}

	if !before.IsZero() {
		for _, txn := range t {
			if txn.Date.Before(before) {
				filteredTransactions = append(filteredTransactions, txn)
			}
		}

		return filteredTransactions, nil
	}

	if !after.IsZero() {
		for _, txn := range t {
			if txn.Date.After(after) {
				filteredTransactions = append(filteredTransactions, txn)
			}
		}

		return filteredTransactions, nil
	}

	return nil, fmt.Errorf("something wrong with the dates")
}

// func (t Transactions) FilterByAccount(accountName string) (Transactions, error) {
// 	return t, nil
// }
