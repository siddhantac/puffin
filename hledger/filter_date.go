package hledger

import (
	"fmt"
	"time"
)

type DateFilter struct {
	From      string
	To        string
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
	now := time.Now()
	month := now.Month()
	year := now.Year()
	prevMonth := int(month) - n
	if prevMonth < 0 {
		year = year - 1
		prevMonth = 12 - prevMonth - 1
	}

	smartDate := fmt.Sprintf("%v/%d..", year, prevMonth)

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
