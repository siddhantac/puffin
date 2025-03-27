package interfaces

type DataProvider interface {
	AccountBalances() ([][]string, error)
	SubAccountBalances(accountType, account, from, to string) ([][]string, error)
	Records(account, from, to string) ([][]string, error)
}
