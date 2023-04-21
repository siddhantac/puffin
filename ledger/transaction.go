package ledger

import (
	"fmt"
	"time"

	"github.com/howeyc/ledger/decimal"
)

type Transactions []*Transaction

// Account holds the name and balance
type Account struct {
	Name    string
	Balance decimal.Decimal
	Comment string
}

func (a Account) String() string {
	return fmt.Sprintf("%s\t%v", a.Name, a.Balance.StringFixedBank())
}

// Transaction is the basis of a ledger. The ledger holds a list of transactions.
// A Transaction has a Payee, Date (with no time, or to put another way, with
// hours,minutes,seconds values that probably doesn't make sense), and a list of
// Account values that hold the value of the transaction for each account.
type Transaction struct {
	Date           time.Time
	Payee          string
	PayeeComment   string
	AccountChanges []Account
	Comments       []string
}

func (t Transaction) String() string {
	s := fmt.Sprintf("%v  %s\n", t.Date.Format("2006-01-02"), t.Payee)
	for _, acc := range t.AccountChanges {
		account := fmt.Sprintf("%s\t%v", acc.Name, acc.Balance)
		s = fmt.Sprintf("%s\t\t%s\n", s, account)
	}

	return s
}
