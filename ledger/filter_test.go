package ledger

import (
	"testing"
	"time"

	"github.com/howeyc/ledger/decimal"
	"github.com/stretchr/testify/assert"
)

// func TestFilterByAccount(t *testing.T) {
// 	txns := Transactions{
// 		{
// 			Date:  time.Now(),
// 			Payee: "test payee",
// 			AccountChanges: []Account{
// 				{
// 					Name:    "expenses:rent",
// 					Balance: decimal.NewFromFloat(150.23),
// 				},
// 				{
// 					Name:    "income:salary",
// 					Balance: decimal.NewFromFloat(-150.23),
// 				},
// 			},
// 		},
// 		{
// 			Date:  time.Now().Add(-48 * time.Hour),
// 			Payee: "test payee 2",
// 			AccountChanges: []Account{
// 				{
// 					Name:    "expenses:food",
// 					Balance: decimal.NewFromFloat(20.50),
// 				},
// 				{
// 					Name:    "income:interest",
// 					Balance: decimal.NewFromFloat(-20.50),
// 				},
// 			},
// 		},
// 	}
//
// 	tests := map[string]struct {
// 		accountName        string
// 		assertError        func(*testing.T, error)
// 		assertTransactions func(*testing.T, Transactions)
// 	}{
// 		"when filtering by account 'expenses' then only return expense transactions": {
// 			accountName: "expenses",
// 			assertError: func(t *testing.T, err error) { assert.NoError(t, err) },
// 			assertTransactions: func(t *testing.T, gotTxns Transactions) {
// 				assert.Equal(t, txns, gotTxns)
// 			},
// 		},
// 	}
//
// 	for name, test := range tests {
// 		t.Run(name, func(t *testing.T) {
// 			gotTxns, err := txns.FilterByAccount(test.accountName)
// 			test.assertError(t, err)
// 			test.assertTransactions(t, gotTxns)
// 		})
// 	}
// }

func TestFilterByDate(t *testing.T) {
	txns := Transactions{
		{
			Date:  time.Now(),
			Payee: "test payee",
			AccountChanges: []Account{
				{
					Name:    "expenses:rent",
					Balance: decimal.NewFromFloat(150.23),
				},
				{
					Name:    "income:salary",
					Balance: decimal.NewFromFloat(-150.23),
				},
			},
		},
		{
			Date:  time.Now().Add(-48 * time.Hour),
			Payee: "test payee 2",
			AccountChanges: []Account{
				{
					Name:    "expenses:food",
					Balance: decimal.NewFromFloat(20.50),
				},
				{
					Name:    "income:interest",
					Balance: decimal.NewFromFloat(-20.50),
				},
			},
		},
	}

	tests := map[string]struct {
		before             time.Time
		after              time.Time
		assertError        func(*testing.T, error)
		assertTransactions func(*testing.T, Transactions)
	}{
		"all transactions are between before and after": {
			after:       time.Now().Add(-72 * time.Hour),
			before:      time.Now().Add(24 * time.Hour),
			assertError: func(t *testing.T, err error) { assert.NoError(t, err) },
			assertTransactions: func(t *testing.T, gotTxns Transactions) {
				assert.Equal(t, txns, gotTxns)
			},
		},
		"only 1 transaction is within before and after": {
			after:       time.Now().Add(-24 * time.Hour),
			before:      time.Now().Add(1 * time.Hour),
			assertError: func(t *testing.T, err error) { assert.NoError(t, err) },
			assertTransactions: func(t *testing.T, gotTxns Transactions) {
				assert.Len(t, gotTxns, 1)
				assert.Equal(t, txns[0], gotTxns[0])
			},
		},
		"only after is provided": {
			after:       time.Now().Add(-24 * time.Hour),
			assertError: func(t *testing.T, err error) { assert.NoError(t, err) },
			assertTransactions: func(t *testing.T, gotTxns Transactions) {
				assert.Len(t, gotTxns, 1)
				assert.Equal(t, txns[0], gotTxns[0])
			},
		},
		"only before is provided": {
			before:      time.Now().Add(-24 * time.Hour),
			assertError: func(t *testing.T, err error) { assert.NoError(t, err) },
			assertTransactions: func(t *testing.T, gotTxns Transactions) {
				assert.Len(t, gotTxns, 1)
				assert.Equal(t, txns[1], gotTxns[0])
			},
		},
		"when no timestamps are provided it returns all transactions": {
			assertError: func(t *testing.T, err error) { assert.NoError(t, err) },
			assertTransactions: func(t *testing.T, gotTxns Transactions) {
				assert.Equal(t, txns, gotTxns)
			},
		},
	}

	for name, test := range tests {
		t.Run(name, func(t *testing.T) {
			gotTxns, err := txns.FilterByDate(test.before, test.after)
			test.assertError(t, err)
			test.assertTransactions(t, gotTxns)
		})
	}
}
