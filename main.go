package main

import (
	"flag"
	"puffin/hledger"
	"puffin/ui"
)

func main() {
	var journalFile string
	flag.StringVar(&journalFile, "file", "", "journal filename")
	flag.Parse()

	var hl hledger.Hledger

	if journalFile != "" {
		hl = hledger.NewWithJournalFile(journalFile)
	} else {
		hl = hledger.New()
	}

	ui.New(hl).Start()
}
