package main

import (
	"flag"
	"puffin/ui"

	hlgo "github.com/siddhantac/hledger"
)

func main() {
	var journalFile, hledgerExecutable string
	flag.StringVar(&journalFile, "file", "", "journal filename")
	flag.StringVar(&hledgerExecutable, "exe", "hledger", "hledger executable")
	flag.Parse()

	hl := hlgo.New(hledgerExecutable, journalFile)
	hlcmd := ui.NewHledgerCmd(hl)

	ui.Start(hlcmd)
}
