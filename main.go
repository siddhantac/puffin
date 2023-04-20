package main

import (
	"fmt"
	"os"

	"puffin/ledger"
)

func main() {
	// hl := hledger.New()
	// ui.New(hl).Start()

	if len(os.Args) != 2 {
		fmt.Println("ledger filename required")
		return
	}

	txns, err := ledger.ParseLedgerFile(os.Args[1])
	if err != nil {
		fmt.Println(err)
		return
	}

	for i, t := range txns {
		fmt.Println(t)
		if i > 10 {
			return
		}
	}
}
