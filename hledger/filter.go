package hledger

import "fmt"

type Filter interface {
	Build() string
}

type NoFilter struct{}

func (NoFilter) Build() string {
	return ""
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
