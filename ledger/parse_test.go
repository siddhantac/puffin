package ledger

import (
	"bytes"
	"encoding/json"
	"errors"
	"strings"
	"sync"
	"testing"
	"time"

	"github.com/howeyc/ledger/decimal"
	"github.com/stretchr/testify/assert"
)

var emptyLedgerData = ``

var ledgerData = `
2021-12-01 Gym
    expenses:fitness                                  $60.00
    liabilities:credit_card:american_express      $-60.00
`

// 2021-12-08 Rent
//     expenses:rent              $2,300.00
//     assets:bank:maybank    $-2,300.00
//
// 2021-12-01 Family expenses
//     expenses:household        $800.00
//     assets:bank:chase     $-800.00
//
// 2021-12-02 Phone
//     expenses:utilities                               $100.54
//     liabilities:credit_card:american_express     $-100.54
//
// 2021-12-03 Adjustment
//     assets:bank:stanchart        $0.05
//     income:others             $-0.05

func TestParse(t *testing.T) {
	testcases := map[string]struct {
		ledgerData         string
		assertError        func(*testing.T, error)
		assertTransactions func(*testing.T, []*Transaction)
	}{
		"empty ledger": {
			ledgerData: emptyLedgerData,
			assertError: func(t *testing.T, err error) {
				assert.NoError(t, err)
			},
			assertTransactions: func(t *testing.T, transactions []*Transaction) {
				assert.Len(t, transactions, 0)
			},
		},
		"correct ledger": {
			ledgerData: ledgerData,
			assertError: func(t *testing.T, err error) {
				assert.NoError(t, err)
			},
			assertTransactions: func(t *testing.T, transactions []*Transaction) {
				expected := []*Transaction{
					{
						Date:  time.Date(2021, 12, 1, 0, 0, 0, 0, &time.Location{}),
						Payee: "Gym",
						AccountChanges: []Account{
							{
								Name:    "expenses:fitness",
								Balance: decimal.NewFromInt(60),
							},
							{
								Name:    "liabilities:credit_card:american_express",
								Balance: decimal.NewFromInt(-60),
							},
						},
					},
				}

				assert.Equal(t, expected, transactions)
			},
		},
	}

	for name, tc := range testcases {
		t.Run(name, func(t *testing.T) {
			ledgerReader := strings.NewReader(tc.ledgerData)
			txns, err := ParseLedger(ledgerReader)
			tc.assertError(t, err)
			tc.assertTransactions(t, txns)
		})
	}
}

type testCase struct {
	name         string
	data         string
	transactions []*Transaction
	err          error
}

var testCases = []testCase{
	{
		"simple",
		`1970/01/01 Payee
	Expense/test  (123 * 3)
	Assets
`,
		[]*Transaction{
			{
				Payee: "Payee",
				Date:  time.Unix(0, 0).UTC(),
				AccountChanges: []Account{
					{
						Name:    "Expense/test",
						Balance: decimal.NewFromFloat(369.0),
					},
					{
						Name:    "Assets",
						Balance: decimal.NewFromFloat(-369.0),
					},
				},
			},
		},
		nil,
	},
	{
		"bad payee line",
		`1970/01/01Payee
	Expense/test  (123 * 3)
	Assets      123
`,
		nil,
		errors.New(":1: Unable to parse transaction: Unable to parse payee line: 1970/01/01Payee"),
	},
	{
		"bad payee date",
		`1970/02/31 Payee
	Expense/test  (123 * 3)
	Assets      123
`,
		nil,
		errors.New(`:1: Unable to parse transaction: Unable to parse date(1970/02/31): parsing time "1970/02/31": extra text: "1970/02/31"`),
	},
	{
		"unbalanced error",
		`1970/01/01 Payee
	Expense/test  (123 * 3)
	Assets      123
`,
		nil,
		errors.New(":3: Unable to parse transaction: Unable to balance transaction: no empty account to place extra balance"),
	},
	{
		"single posting",
		`1970/01/01 Payee
	Assets:Account    5`,
		nil,
		errors.New(":2: Unable to parse transaction: Unable to balance transaction: need at least two postings"),
	},
	{
		"no posting",
		`1970/01/01 Payee
`,
		nil,
		errors.New(":1: Unable to parse transaction: Unable to balance transaction: need at least two postings"),
	},
	{
		"multiple empty",
		`1970/01/01 Payee
	Expense/test  (123 * 3)
	Wallet
	Assets      123
	Bank
`,
		nil,
		errors.New(":5: Unable to parse transaction: Unable to balance transaction: more than one account empty"),
	},
	{
		"multiple empty lines",
		`1970/01/01 Payee
	Expense/test  (123 * 3)
	Assets



1970/01/01 Payee
	Expense/test   123
	Assets
`,
		[]*Transaction{
			{
				Payee: "Payee",
				Date:  time.Unix(0, 0).UTC(),
				AccountChanges: []Account{
					{
						Name:    "Expense/test",
						Balance: decimal.NewFromFloat(369.0),
					},
					{
						Name:    "Assets",
						Balance: decimal.NewFromFloat(-369.0),
					},
				},
			},
			{
				Payee: "Payee",
				Date:  time.Unix(0, 0).UTC(),
				AccountChanges: []Account{
					{
						Name:    "Expense/test",
						Balance: decimal.NewFromFloat(123.0),
					},
					{
						Name:    "Assets",
						Balance: decimal.NewFromFloat(-123.0),
					},
				},
			},
		},
		nil,
	},
	{
		"accounts with spaces",
		`1970/01/02 Payee
 Expense:test	369.0
 Assets

; Handle tabs between account and amount
; Also handle accounts with spaces
1970/01/01 Payee 5
	Expense:Cars R Us
	Expense:Cars  358.0
	Expense:Cranks	10
	Expense:Cranks Unlimited	10
	Expense:Cranks United  10
`,
		[]*Transaction{
			{
				Payee: "Payee",
				Date:  time.Unix(0, 0).UTC().AddDate(0, 0, 1),
				AccountChanges: []Account{
					{
						Name:    "Expense:test",
						Balance: decimal.NewFromFloat(369.0),
					},
					{
						Name:    "Assets",
						Balance: decimal.NewFromFloat(-369.0),
					},
				},
			},
			{
				Payee: "Payee 5",
				Date:  time.Unix(0, 0).UTC(),
				AccountChanges: []Account{
					{
						Name:    "Expense:Cars R Us",
						Balance: decimal.NewFromFloat(-388.0),
					},
					{
						Name:    "Expense:Cars",
						Balance: decimal.NewFromFloat(358.0),
					},
					{
						Name:    "Expense:Cranks",
						Balance: decimal.NewFromFloat(10.0),
					},
					{
						Name:    "Expense:Cranks Unlimited",
						Balance: decimal.NewFromFloat(10.0),
					},
					{
						Name:    "Expense:Cranks United",
						Balance: decimal.NewFromFloat(10.0),
					},
				},
				Comments: []string{
					"; Handle tabs between account and amount",
					"; Also handle accounts with spaces",
				},
			},
		},
		nil,
	},
	{
		"accounts with slashes",
		`1970-01-01 Payee
    Expense/another     5
	Expense/test
	Assets      -128
`,
		[]*Transaction{
			{
				Payee: "Payee",
				Date:  time.Unix(0, 0).UTC(),
				AccountChanges: []Account{
					{
						Name:    "Expense/another",
						Balance: decimal.NewFromFloat(5.0),
					},
					{
						Name:    "Expense/test",
						Balance: decimal.NewFromFloat(123.0),
					},
					{
						Name:    "Assets",
						Balance: decimal.NewFromFloat(-128.0),
					},
				},
			},
		},
		nil,
	},
	{
		"comment after payee",
		`; before trans
1970-01-01 Payee      ; payee comment
	Expense/test  123
	Assets
`,
		[]*Transaction{
			{
				Payee:        "Payee",
				Date:         time.Unix(0, 0).UTC(),
				PayeeComment: "; payee comment",
				AccountChanges: []Account{
					{
						Name:    "Expense/test",
						Balance: decimal.NewFromFloat(123.0),
					},
					{
						Name:    "Assets",
						Balance: decimal.NewFromFloat(-123.0),
					},
				},
				Comments: []string{
					"; before trans",
				},
			},
		},
		nil,
	},
	{
		"comment inside transaction",
		`1970-01-01 Payee
	Expense/test  123
	; Expense/test  123
	Assets
`,
		[]*Transaction{
			{
				Payee: "Payee",
				Date:  time.Unix(0, 0).UTC(),
				AccountChanges: []Account{
					{
						Name:    "Expense/test",
						Balance: decimal.NewFromFloat(123.0),
					},
					{
						Name:    "Assets",
						Balance: decimal.NewFromFloat(-123.0),
					},
				},
				Comments: []string{
					"; Expense/test  123",
				},
			},
		},
		nil,
	},
	{
		"multiple comments",
		`; comment
	1970/01/01 Payee
	Expense/test   58
	Assets         -58           ; comment in trans
	Expense/unbalanced
`,
		[]*Transaction{
			{
				Payee: "Payee",
				Date:  time.Unix(0, 0).UTC(),
				AccountChanges: []Account{
					{
						Name:    "Expense/test",
						Balance: decimal.NewFromFloat(58),
					},
					{
						Name:    "Assets",
						Balance: decimal.NewFromFloat(-58),
						Comment: "; comment in trans",
					},
					{
						Name:    "Expense/unbalanced",
						Balance: decimal.NewFromFloat(0),
					},
				},
				Comments: []string{
					"; comment",
				},
			},
		},
		nil,
	},
	{
		"header comment",
		`; comment
	1970/01/01 Payee
	Expense/test   58
	Assets         -58
	Expense/test   158
	Assets         -158
`,
		[]*Transaction{
			{
				Payee: "Payee",
				Date:  time.Unix(0, 0).UTC(),
				AccountChanges: []Account{
					{
						Name:    "Expense/test",
						Balance: decimal.NewFromFloat(58),
					},
					{
						Name:    "Assets",
						Balance: decimal.NewFromFloat(-58),
					},
					{
						Name:    "Expense/test",
						Balance: decimal.NewFromFloat(158),
					},
					{
						Name:    "Assets",
						Balance: decimal.NewFromFloat(-158),
					},
				},
				Comments: []string{
					"; comment",
				},
			},
		},
		nil,
	},
	{
		"account skip",
		`1970/01/01 Payee
	Expense/test  123
	Assets

account Expense/test

account Assets
	note bambam
	payee junkjunk

1970/01/01 Payee
	Expense/test  (123 * 2)
	Assets
`,
		[]*Transaction{
			{
				Payee: "Payee",
				Date:  time.Unix(0, 0).UTC(),
				AccountChanges: []Account{
					{
						Name:    "Expense/test",
						Balance: decimal.NewFromFloat(123.0),
					},
					{
						Name:    "Assets",
						Balance: decimal.NewFromFloat(-123.0),
					},
				},
			},
			{
				Payee: "Payee",
				Date:  time.Unix(0, 0).UTC(),
				AccountChanges: []Account{
					{
						Name:    "Expense/test",
						Balance: decimal.NewFromFloat(246.0),
					},
					{
						Name:    "Assets",
						Balance: decimal.NewFromFloat(-246.0),
					},
				},
			},
		},
		nil,
	},
	{
		"multiple account skip",
		`1970/01/01 Payee
	Expense/test  123
	Assets

account Banking
account Expense/test
account Assets

1970/01/01 Payee
	Expense/test  (123 * 2)
	Assets
`,
		[]*Transaction{
			{
				Payee: "Payee",
				Date:  time.Unix(0, 0).UTC(),
				AccountChanges: []Account{
					{
						Name:    "Expense/test",
						Balance: decimal.NewFromFloat(123.0),
					},
					{
						Name:    "Assets",
						Balance: decimal.NewFromFloat(-123.0),
					},
				},
			},
			{
				Payee: "Payee",
				Date:  time.Unix(0, 0).UTC(),
				AccountChanges: []Account{
					{
						Name:    "Expense/test",
						Balance: decimal.NewFromFloat(246.0),
					},
					{
						Name:    "Assets",
						Balance: decimal.NewFromFloat(-246.0),
					},
				},
			},
		},
		nil,
	},
}

func TestParseLedger(t *testing.T) {
	for _, tc := range testCases {
		b := bytes.NewBufferString(tc.data)
		transactions, err := ParseLedgerOLD(b)
		if (err != nil && tc.err == nil) || (err != nil && tc.err != nil && err.Error() != tc.err.Error()) {
			t.Errorf("Error: expected `%s`, got `%s`", tc.err, err)
		}
		exp, _ := json.Marshal(tc.transactions)
		got, _ := json.Marshal(transactions)
		if string(exp) != string(got) {
			t.Errorf("Error(%s): expected \n`%s`, \ngot \n`%s`", tc.name, exp, got)
		}
	}
}

func TestParseLedgerAsync(t *testing.T) {
	buf := bytes.NewBufferString(`; test
account bam:bam
	subacc line  ; sub comment
	another subacc line

1970/01/01 Payee
	Assets       50
	Expenses

1970/02/30 Error  ; oops
	Assets   30
	Expenses

1970/01/01bbafafdaf;bad comment
	Assets 20
	Expenses

account endofledger`)

	tc, ec := ParseLedgerAsync(buf)

	var trans []*Transaction
	var errors []error

	var wg sync.WaitGroup
	wg.Add(2)
	go func() {
		for t := range tc {
			trans = append(trans, t)
		}
		wg.Done()
	}()
	go func() {
		for e := range ec {
			errors = append(errors, e)
		}
		wg.Done()
	}()
	wg.Wait()

	if len(trans) < 1 || len(errors) < 1 {
		t.Error("async parse failed")
	}
}

func BenchmarkParseLedger(b *testing.B) {
	for n := 0; n < b.N; n++ {
		_, _ = ParseLedgerFile("testdata/ledgerBench.dat")
	}
}
