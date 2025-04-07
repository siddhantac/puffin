package interfaces

type Filter interface {
	DateStart() string
	DateEnd() string
	AccountName() string
	Description() string
}

type DataProvider interface {
	AccountBalances() ([][]string, error)
	SubAccountBalances(accountType, account, from, to string) ([][]string, error)
	Records(account string, filter Filter) ([][]string, error)
}
