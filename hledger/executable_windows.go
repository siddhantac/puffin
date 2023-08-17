//go:build windows

package hledger

func hledgerExecutable() string {
	return filepath.Join(getCwd(), "hledger.exe")
}

func getCwd() string {
	ex, err := os.Executable()
	if err != nil {
		panic(err)
	}
	return filepath.Dir(ex)
}
