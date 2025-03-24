package interfaces

type DataProvider interface {
	AccountBalances() ([][]string, error)
	SubAccountBalances(string, string) ([][]string, error)
	Records(string) ([][]string, error)
}
