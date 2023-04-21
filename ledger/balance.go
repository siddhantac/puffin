package ledger

import (
	"sort"

	"github.com/howeyc/ledger/decimal"
)

func (t Transactions) Balance() []*Account {
	balances := make(map[string]decimal.Decimal)
	for _, txn := range t {
		for _, change := range txn.AccountChanges {
			if bal, ok := balances[change.Name]; !ok {
				balances[change.Name] = change.Balance
			} else {
				balances[change.Name] = bal.Add(change.Balance)
			}
		}
	}

	accounts := make([]*Account, 0, len(balances))
	for accName, bal := range balances {
		accounts = append(accounts, &Account{Name: accName, Balance: bal})
	}

	sort.Slice(accounts, func(i, j int) bool {
		return accounts[i].Name < accounts[j].Name
	})

	return accounts
}
