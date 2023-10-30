package hledger

import "fmt"

type DropAccountFilter struct {
	dropValue int
}

func NewDropAccountFilter() DropAccountFilter {
	return DropAccountFilter{dropValue: 1}
}

func (af DropAccountFilter) Name() string { return "drop_account_filter" }

func (af DropAccountFilter) Build() string {
	return fmt.Sprintf("--drop=%d", af.dropValue)
}

func (af DropAccountFilter) Value() string { return fmt.Sprintf("%d", af.dropValue) }
