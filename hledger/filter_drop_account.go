package hledger

type DropAccountFilter struct {
}

func NewDropAccountFilter() DropAccountFilter {
	return DropAccountFilter{}
}

func (af DropAccountFilter) Name() string { return "drop_account_filter" }

func (af DropAccountFilter) Build() string {
	return "--drop 1"
}
