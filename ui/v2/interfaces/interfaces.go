package interfaces

type Filter struct {
	AccountType string
	Account     string
	DateStart   string
	DateEnd     string
	Description string
}

type DataProvider interface {
	AccountBalances() ([][]string, error)
	SubAccountBalances(filter Filter) ([][]string, error)
	Records(filter Filter) ([][]string, error)
	IncomeStatement(filter Filter) ([]byte, error)
	BalanceSheet(filter Filter) ([]byte, error)
	IncomeStatement2(filter Filter) (*ComplexTable, error)
	BalanceSheet2(filter Filter) (*ComplexTable, error)
}

type ComplexTable struct {
	Title        string
	LowerTitle   string
	UpperTitle   string
	BottomBar    []string
	Columns      []string
	Upper, Lower [][]string
}
