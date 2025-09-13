package main

import (
	"flag"
	"log"

	"github.com/siddhantac/puffin/ui"
	v3 "github.com/siddhantac/puffin/ui/v2"
)

func main() {
	var isDebug, runV3 bool
	var configFile string

	flag.StringVar(&configFile, "cfg", "", "config file")
	flag.BoolVar(&isDebug, "debug", false, "run in debug mode")
	flag.BoolVar(&runV3, "v3", false, "run v3")
	flag.Parse()

	if runV3 {
		var cfg v3.Config
		if configFile != "" {
			var err error
			cfg, err = v3.NewConfig(configFile)
			if err != nil {
				log.Fatal(err)
			}
		}
		v3.Start(cfg, isDebug)
	} else {
		cfg := ui.DefaultConfig
		if configFile != "" {
			var err error
			cfg, err = ui.NewConfig(configFile)
			if err != nil {
				log.Fatal(err)
			}
		}
		ui.Start(cfg, isDebug)
	}
}
