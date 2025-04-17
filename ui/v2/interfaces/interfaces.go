package interfaces

type Filter struct {
	AccountType string
	Account     string
	DateStart   string
	DateEnd     string
	Description string
}

type DisplayOptions struct {
	Interval string
}

type DataProvider interface {
	AccountBalances() ([][]string, error)
	SubAccountBalances(filter Filter) ([][]string, error)
	Records(filter Filter) ([][]string, error)
	IncomeStatement(filter Filter, displayOptions DisplayOptions) (*ComplexTable, error)
	BalanceSheet(filter Filter) (*ComplexTable, error)
}

type ComplexTable struct {
	Title        string
	LowerTitle   string
	UpperTitle   string
	BottomBar    []string
	Columns      []string
	Upper, Lower [][]string
}
