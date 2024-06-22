package main

import (
	"flag"
	"log"
	"puffin/accounting"
	"puffin/ui"

	hlgo "github.com/siddhantac/hledger"
)

func main() {
	var journalFile, hledgerExecutable string
	var isDebug bool
	var configFile string

	flag.StringVar(&configFile, "cfg", "", "config file")
	flag.StringVar(&journalFile, "file", "", "journal filename")
	flag.StringVar(&hledgerExecutable, "exe", "hledger", "hledger executable")
	flag.BoolVar(&isDebug, "debug", false, "run in debug mode")
	flag.Parse()

	cfg := ui.DefaultConfig
	if configFile != "" {
		var err error
		cfg, err = ui.NewConfig(configFile)
		if err != nil {
			log.Fatal(err)
		}
	}

	hl := hlgo.New(hledgerExecutable, journalFile)
	hlcmd := accounting.NewHledgerCmd(hl)

	ui.Start(hlcmd, cfg, isDebug)
}
