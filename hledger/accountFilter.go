package hledger

import "fmt"

type AccountFilter struct {
	account string
	invert  bool
}

// NewSimpleAccountFilter only shows expenses,income and investments. It filters
// out bank transactions and liabilities (credit-cards)
func NewSimpleAccountFilter() AccountFilter {
	return NewAccountFilter("assets:bank|liabilities").Invert()
}

func NewAccountFilter(account string) AccountFilter {
	return AccountFilter{account: account}
}

func (af AccountFilter) Invert() AccountFilter {
	af.invert = true
	return af
}

func (af AccountFilter) Name() string { return "name_filter" }

func (af AccountFilter) Build() string {
	if af.account == "" {
		return ""
	}
	base := fmt.Sprintf("acct:\"%s\" ", af.account)
	if af.invert {
		return fmt.Sprintf("not:%s", base)
	}
	return fmt.Sprintf(" %s", base)
}
