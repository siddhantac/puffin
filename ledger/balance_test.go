package ledger

import (
	"strings"
	"testing"

	"github.com/howeyc/ledger/decimal"
	"github.com/stretchr/testify/assert"
)

func TestBalance(t *testing.T) {
	txns, err := ParseLedger(strings.NewReader(validLedgerData))
	assert.NoError(t, err)
	balances := txns.Balance(0)

	expected := []*Account{
		{
			Name:    "assets:bank:chase",
			Balance: decimal.NewFromInt(-800),
		},
		{
			Name:    "assets:bank:maybank",
			Balance: decimal.NewFromInt(-2300),
		},
		{
			Name:    "expenses:fitness",
			Balance: decimal.NewFromInt(60),
		},
		{
			Name:    "expenses:household",
			Balance: decimal.NewFromInt(3100),
		},
		{
			Name:    "liabilities:credit_card:american_express",
			Balance: decimal.NewFromInt(-60),
		},
	}

	assert.Equal(t, expected, balances)
}

func TestBalanceWithDepth(t *testing.T) {
	txns, err := ParseLedger(strings.NewReader(validLedgerData))
	assert.NoError(t, err)

	depth := 2
	balances := txns.Balance(depth)

	expected := []*Account{
		{
			Name:    "assets:bank",
			Balance: decimal.NewFromInt(-3100),
		},
		{
			Name:    "expenses:fitness",
			Balance: decimal.NewFromInt(60),
		},
		{
			Name:    "expenses:household",
			Balance: decimal.NewFromInt(3100),
		},
		{
			Name:    "liabilities:credit_card:american_express",
			Balance: decimal.NewFromInt(-60),
		},
	}

	assert.Equal(t, expected, balances)
}
