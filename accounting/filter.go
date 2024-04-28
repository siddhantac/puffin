package accounting

type Filter interface {
	Name() string
	Build() string
	Value() string
}

type FilterType int

const (
	FilterTypeAccount FilterType = iota
	FilterTypeDate
	FilterTypePeriod
	FilterTypeAccountDepth
	FilterTypeStartDate
	FilterTypeEndDate
	FilterTypeDescription
	FilterTypeDropAccount
)

func NewFilter(filterType FilterType, value interface{}) Filter {
	switch filterType {
	case FilterTypeAccount:
		return NewAccountFilter(value.(string))
	case FilterTypeDate:
		return NewDateFilter()
	case FilterTypeAccountDepth:
		return NewAccountDepthFilter()
	case FilterTypeStartDate:
		return NewStartDateFilter(value.(string))
	case FilterTypeEndDate:
		return NewEndDateFilter(value.(string))
	case FilterTypeDescription:
		return NewDescriptionFilter(value.(string))
	case FilterTypeDropAccount:
		return NewDropAccountFilter()
	default:
		return NoFilter{}
	}
}
