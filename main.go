package main

import (
	"hledger/hledger"
	"hledger/ui"
)

// TODO
// - cache layer
// - form to enter filter, date etc

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
