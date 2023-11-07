package hledger

type PeriodFilter struct {
	period string
}

func NewPeriodFilter() PeriodFilter {
	return PeriodFilter{}
}

func (pf PeriodFilter) Name() string { return "period_filter" }

func (pf PeriodFilter) Monthly() PeriodFilter {
	pf.period = "-M"
	return pf
}

func (pf PeriodFilter) Quarterly() PeriodFilter {
	pf.period = "-Q"
	return pf
}

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

func (pf PeriodFilter) Value() string { return pf.period }
