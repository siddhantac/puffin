package hledger

import "fmt"

type DescriptionFilter struct {
	Query string
}

func NewDescriptionFilter(query string) DescriptionFilter {
	return DescriptionFilter{
		Query: query,
	}
}

func (d DescriptionFilter) Name() string { return "description_filter" }

func (d DescriptionFilter) Build() string {
	return fmt.Sprintf(" desc:\"%s\"", d.Query)
}

func (d DescriptionFilter) Value() string { return d.Query }
