package main

import (
	"fmt"
	"os"

	"puffin/ledger"
	"puffin/report"
)

func main() {
	// hl := hledger.New()
	// ui.New(hl).Start()

	if len(os.Args) != 2 {
		fmt.Println("ledger filename required")
		return
	}

	f, err := os.Open(os.Args[1])
	if err != nil {
		fmt.Println(err)
		return
	}
	defer f.Close()

	txns, err := ledger.ParseLedger(f)
	if err != nil {
		fmt.Println(err)
		return
	}

	if err = report.NewMonthlyReport(txns); err != nil {
		fmt.Println(err)
		return
	}
}
