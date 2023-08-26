package hledger

import "fmt"

type DateFilter struct {
	From      string
	To        string
	SmartText string
}

func NewDateFilter() DateFilter {
	return DateFilter{}
}

func (d DateFilter) LastMonth() DateFilter {
	return d.WithSmartDate("last month")
}

func (d DateFilter) UpToToday() DateFilter {
	return d.WithSmartDate("..today")
}

func (d DateFilter) WithSmartDate(text string) DateFilter {
	d.SmartText = text
	return d
}

func (d DateFilter) Name() string { return "date_filter" }

func (d DateFilter) Build() string {
	if d.SmartText != "" {
		return fmt.Sprintf("date:%s", d.SmartText)
	}
	return ""
}
