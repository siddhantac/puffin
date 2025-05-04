package interfaces

type Filter struct {
	AccountType string
	Account     string
	DateStart   string
	DateEnd     string
	Description string
}

type DisplayOptions struct {
	Interval Interval
	Depth    int
	Sort     SortBy
}

type Interval string

type SortBy string

const (
	Monthly Interval = "monthly"
	Yearly  Interval = "yearly"

	ByAmount  SortBy = "amount"
	ByAccount SortBy = "account"
)

type DataProvider interface {
	Balance(filter Filter, displayOptions DisplayOptions) ([][]string, error)
	Records(filter Filter) ([][]string, error)
	IncomeStatement(filter Filter, displayOptions DisplayOptions) (*ComplexTable, error)
	BalanceSheet(filter Filter, displayOptions DisplayOptions) (*ComplexTable, error)
}

type ComplexTable struct {
	Title        string
	LowerTitle   string
	UpperTitle   string
	BottomBar    []string
	Columns      []string
	Upper, Lower [][]string
}
