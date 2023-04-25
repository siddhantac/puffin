package main

import (
	"fmt"
	"os"
	"time"

	"puffin/ledger"
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

	filtered, err := txns.FilterByDate(time.Date(2022, 1, 1, 0, 0, 0, 0, &time.Location{}), time.Time{})
	if err != nil {
		fmt.Println(err)
		return
	}

	for i, t := range filtered {
		fmt.Println(t)
		if i > 10 {
			return
		}
	}
}
