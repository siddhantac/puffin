package accounting

import "fmt"

type StartDateFilter struct {
	startDate string
}

func NewStartDateFilter(startDate string) StartDateFilter {
	return StartDateFilter{startDate: startDate}
}

func (sd StartDateFilter) Name() string { return "start_date_filter" }

func (sd StartDateFilter) Build() string {
	if sd.startDate == "" {
		return ""
	}
	return fmt.Sprintf("-b %s", sd.startDate)
}

func (sd StartDateFilter) Value() string { return sd.startDate }

type EndDateFilter struct {
	endDate string
}

func NewEndDateFilter(endDate string) EndDateFilter {
	return EndDateFilter{endDate: endDate}
}

func (ed EndDateFilter) Name() string { return "end_date_filter" }

func (ed EndDateFilter) Build() string {
	if ed.endDate == "" {
		return ""
	}
	return fmt.Sprintf("-e %s", ed.endDate)
}

func (ed EndDateFilter) Value() string { return ed.endDate }
