package accounting

type NoFilter struct{}

func (NoFilter) Name() string  { return "no_filter" }
func (NoFilter) Build() string { return "" }
func (NoFilter) Value() string { return "" }
