package hledger

import "fmt"

type AccountDepthFilter struct {
	count int
}

func NewAccountDepthFilter() AccountDepthFilter {
	return AccountDepthFilter{}
}

func (adf AccountDepthFilter) Name() string { return "acct_depth_filter" }

func (adf AccountDepthFilter) Build() string {
	if adf.count == 0 {
		return ""
	}
	return fmt.Sprint(" --depth:%d", adf.count)
}
