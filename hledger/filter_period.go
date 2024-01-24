package accounting

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
func (pf PeriodFilter) RawValue() string {
	switch pf.period {
	case "-M":
		return "--monthly"
	case "-Y":
		return "--yearly"
	}
	return "--yearly"
}
