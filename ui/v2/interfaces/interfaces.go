package interfaces

type Filter struct {
	Account     string
	DateStart   string
	DateEnd     string
	Description string
}

type FilterDeprecated interface {
	DateStart() string
	DateEnd() string
	AccountName() string
	Description() string
}

type DataProvider interface {
	AccountBalances() ([][]string, error)
	SubAccountBalances(filter Filter) ([][]string, error)
	Records(filter Filter) ([][]string, error)
	IncomeStatement(filter Filter) ([]byte, error)
}
