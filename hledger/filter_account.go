package hledger

import "fmt"

type AccountFilter struct {
	account string
}

func NewAccountFilter(account string) AccountFilter {
	return AccountFilter{account: account}
}

func (af AccountFilter) Name() string { return "name_filter" }

func (af AccountFilter) Build() string {
	if af.account == "" {
		return ""
	}
	return fmt.Sprintf("acct:%s", af.account)
}
