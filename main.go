package main

import (
	"flag"
	"puffin/accounting"
	"puffin/ui"

	hlgo "github.com/siddhantac/hledger"
)

func main() {
	var journalFile, hledgerExecutable string
	var isDebug bool
	flag.StringVar(&journalFile, "file", "", "journal filename")
	flag.StringVar(&hledgerExecutable, "exe", "hledger", "hledger executable")
	flag.BoolVar(&isDebug, "debug", false, "run in debug mode")
	flag.Parse()

	hl := hlgo.New(hledgerExecutable, journalFile)
	hlcmd := accounting.NewHledgerCmd(hl)

	ui.Start(hlcmd, isDebug)
}
