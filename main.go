package main

import (
	"flag"
	"puffin/hledger"
	"puffin/ui"
)

func main() {
	var journalFile, hledgerExecutable string
	flag.StringVar(&journalFile, "file", "", "journal filename")
	flag.StringVar(&hledgerExecutable, "exe", "", "hledger executable")
	flag.Parse()

	var hl hledger.Hledger

	if journalFile != "" {
		hl.JournalFilename = journalFile
	}

	if hledgerExecutable != "" {
		hl.HledgerBinary = hledgerExecutable
	}

	ui.New(hl).Start()
}
