package hledger

type PeriodFilter struct {
	period string
}

func NewPeriodFilter() PeriodFilter {
	return PeriodFilter{}
}

func (pf PeriodFilter) Name() string { return "period_filter" }

func (pf PeriodFilter) Yearly() PeriodFilter {
	pf.period = "-Y"
	return pf
}

func (pf PeriodFilter) Build() string {
	if pf.period == "" {
		return ""
	}

	return pf.period
}
