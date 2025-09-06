package main

import (
	"flag"
	"log"

	"github.com/siddhantac/puffin/ui"
	v3 "github.com/siddhantac/puffin/ui/v2"
)

func main() {
	var isDebug bool
	var runV3 bool
	var runV2 bool
	var configFile string

	flag.StringVar(&configFile, "cfg", "", "config file")
	flag.BoolVar(&isDebug, "debug", false, "run in debug mode")
	// Default to V3 (current) UI. Use -v2 to run legacy UI.
	flag.BoolVar(&runV3, "v3", true, "run v3 (current)")
	flag.BoolVar(&runV2, "v2", false, "run v2 (legacy)")
	flag.Parse()

	if runV2 {
		cfg := ui.DefaultConfig
		if configFile != "" {
			var err error
			cfg, err = ui.NewConfig(configFile)
			if err != nil {
				log.Fatal(err)
			}
		}
		ui.Start(cfg, isDebug)
		return
	}

	// Default path: run V3
	v3.Start(isDebug)
}
