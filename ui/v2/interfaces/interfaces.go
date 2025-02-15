package interfaces

type DataProvider interface {
	AccountBalances() ([][]string, error)
	SubAccountBalances(string) ([][]string, error)
}
