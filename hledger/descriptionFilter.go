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

func (d DescriptionFilter) Build() string {
	return fmt.Sprintf(" desc:\"%s\"", d.Query)
}
