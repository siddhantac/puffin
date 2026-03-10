package main

import (
	"flag"
	"fmt"
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
		defer func() {
			if r := recover(); r != nil {
				fmt.Println("Recovered. Error:\n", r)
			}
		}()
		v3.Start(isDebug)
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
