package ledger

import (
	"sort"

	"github.com/howeyc/ledger/decimal"
)

func (t Transactions) Balance(depth int) []*Account {
	depth = depth - 1
	balances := make(map[string]decimal.Decimal)
	for _, txn := range t {
		for _, change := range txn.AccountChanges {
			idx := depth
			if depth < 1 {
				idx = len(change.SubAccounts) - 1
			}
			if bal, ok := balances[change.SubAccounts[idx]]; !ok {
				balances[change.SubAccounts[idx]] = change.Balance
			} else {
				balances[change.SubAccounts[idx]] = bal.Add(change.Balance)
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
