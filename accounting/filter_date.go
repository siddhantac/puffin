package accounting

import (
	"fmt"
	"time"
)

type DateFilter struct {
	SmartText string
}

func NewDateFilter() DateFilter {
	return DateFilter{}
}

func (d DateFilter) ThisYear() DateFilter {
	return d.WithSmartDate("this year")
}

func (d DateFilter) LastMonth() DateFilter {
	return d.WithSmartDate("last month")
}

func (d DateFilter) LastNMonths(n int) DateFilter {
	date := time.Now().AddDate(0, -n, 0)
	smartDate := fmt.Sprintf("%v/%02d..", date.Year(), date.Month())
	return d.WithSmartDate(smartDate)
}

func (d DateFilter) LastNYears(n int) DateFilter {
	date := time.Now().AddDate(-n, 0, 0)
	smartDate := fmt.Sprintf("%d..", date.Year())
	return d.WithSmartDate(smartDate)
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

func (d DateFilter) Value() string { return d.SmartText }
