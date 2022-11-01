package main

import (
	"puffin/hledger"
	"puffin/ui"
)

func main() {
	hl := hledger.New()
	ui.New(hl).Start()
}
