package hledger

import "fmt"

type Filter interface {
	Name() string
	Build() string
}

type NoFilter struct{}

func (NoFilter) Name() string  { return "no_filter" }
func (NoFilter) Build() string { return "" }

// date, amount, description
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
	return fmt.Sprintf(" acct:%s ", af.account)
}
