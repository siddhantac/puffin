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
	return d.WithSmartText("last month")
}

func (d DateFilter) UpToToday() DateFilter {
	return d.WithSmartText("..today")
}

func (d DateFilter) WithSmartText(text string) DateFilter {
	d.SmartText = text
	return d
}

func (d DateFilter) WithFromDate(from string) DateFilter {
	d.From = from
	return d
}

func (d DateFilter) WithToDate(to string) DateFilter {
	d.To = to
	return d
}

func (d DateFilter) Build() string {
	if d.SmartText != "" {
		return fmt.Sprintf(" date:\"%s\" ", d.SmartText)
	}

	return fmt.Sprintf(" date:\"%s..%s\" ", d.From, d.To)
}
