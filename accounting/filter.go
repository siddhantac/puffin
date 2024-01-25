package accounting

type Filter interface {
	Name() string
	Build() string
	Value() string
}

type NoFilter struct{}

func (NoFilter) Name() string  { return "no_filter" }
func (NoFilter) Build() string { return "" }
func (NoFilter) Value() string { return "" }
