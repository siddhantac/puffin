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
}
