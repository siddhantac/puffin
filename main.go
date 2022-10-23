package main

import (
	"hledger/hledger"
	"hledger/ui"
)

func main() {
	hl := hledger.New()
	uiobj := ui.New(hl)

	// af := hledger.NewAccountFilter("dbs_twisha")
	// r, err := hl.Register(af)
	// if err != nil {
	// 	fmt.Println(err)
	// 	return
	// }

	uiobj.CreateTable()
}
