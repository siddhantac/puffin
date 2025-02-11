package main

import (
	"flag"
	ui "puffin/ui/v2"
)

func main() {
	var isDebug bool
	var configFile string

	flag.StringVar(&configFile, "cfg", "", "config file")
	flag.BoolVar(&isDebug, "debug", false, "run in debug mode")
	flag.Parse()

	// cfg := ui.DefaultConfig
	// if configFile != "" {
	// 	var err error
	// 	cfg, err = ui.NewConfig(configFile)
	// 	if err != nil {
	// 		log.Fatal(err)
	// 	}
	// }

	// ui.Start(cfg, isDebug)
	ui.Start(isDebug)
}
