package hledger

import "fmt"

type AccountFilter struct {
	account string
}

// NewSimpleAccountFilter only shows expenses,income and investments. It filters
// out bank transactions and liabilities (credit-cards)
func NewSimpleAccountFilter() AccountFilter {
	return NewAccountFilter("^expenses|^income|^assets:inv")
}
func NewAccountFilter(account string) AccountFilter {
	return AccountFilter{account: account}
}

func (af AccountFilter) Name() string { return "name_filter" }

func (af AccountFilter) Build() string {
	if af.account == "" {
		return ""
	}
	return fmt.Sprintf(" acct:\"%s\" ", af.account)
}
