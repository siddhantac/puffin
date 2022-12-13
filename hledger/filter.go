package hledger

type Filter interface {
	Name() string
	Build() string
}

type NoFilter struct{}

func (NoFilter) Name() string  { return "no_filter" }
func (NoFilter) Build() string { return "" }
