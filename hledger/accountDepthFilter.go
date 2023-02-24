package hledger

import "fmt"

type AccountDepthFilter struct {
	count int
}

func NewAccountDepthFilter() AccountDepthFilter {
	return AccountDepthFilter{count: 3}
}

func (adf AccountDepthFilter) Name() string { return "acct_depth_filter" }

func (adf AccountDepthFilter) Build() string {
	if adf.count == 0 {
		return ""
	}
	return fmt.Sprintf(" --depth=%d", adf.count)
}

func (adf AccountDepthFilter) DecreaseDepth(depth int) AccountDepthFilter {
	if adf.count == 1 {
		return adf
	}
	return AccountDepthFilter{count: adf.count - depth}
}

func (adf AccountDepthFilter) IncreaseDepth(depth int) AccountDepthFilter {
	return AccountDepthFilter{count: adf.count + depth}
}

func (adf AccountDepthFilter) SetDepth(depth int) AccountDepthFilter {
	return AccountDepthFilter{count: depth}
}
