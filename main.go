package main

import (
	"hledger/hledger"
	"hledger/ui"
)

func main() {
	hl := hledger.New()
	uiobj := ui.New(hl)

	uiobj.Start()

}
