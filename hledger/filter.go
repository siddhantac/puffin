package hledger

import "fmt"

type Filter interface {
	Build() string
}

// date, amount, description
type AccountFilter struct {
	account string
}

func NewAccountFilter(account string) AccountFilter {
	return AccountFilter{account: account}
}

func (af AccountFilter) Build() string {
	if af.account == "" {
		return ""
	}
	return fmt.Sprintf(" acct:%s ", af.account)
}
