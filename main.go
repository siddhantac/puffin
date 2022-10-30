package main

import (
	"hledger/hledger"
	"hledger/ui"
)

func main() {
	hl := hledger.New()
	ui.New(hl).Start()
}
