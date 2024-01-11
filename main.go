package main

import (
	"flag"
	"puffin/hledger"
	"puffin/ui"

	hlgo "github.com/siddhantac/hledger"
)

func main() {
	var journalFile, hledgerExecutable string
	flag.StringVar(&journalFile, "file", "", "journal filename")
	flag.StringVar(&hledgerExecutable, "exe", "hledger", "hledger executable")
	flag.Parse()

	var hl hledger.Hledger

	if journalFile != "" {
		hl.JournalFilename = journalFile
	}

	if hledgerExecutable != "" {
		hl.HledgerBinary = hledgerExecutable
	}

	hl2 := hlgo.New(hledgerExecutable, journalFile)

	ui.New(hl, hl2).Start()
}
