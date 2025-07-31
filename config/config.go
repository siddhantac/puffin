package config

import (
	"encoding/json"
	"os"
)

type Config struct {
	Home    TabConfig
	Reports TabConfig
}

type TabConfig struct {
	From        string `json:"from"`
	To          string `json:"to"`
	Account     string `json:"account"`
	Description string `json:"description"`
	Depth       int    `json:"depth"`
	Sort        string `json:"sort"`
}

// TODO: add validation
func NewConfig(filename string) (Config, error) {
	var cfg Config

	if filename == "" {
		return cfg, nil
	}

	file, err := os.Open(filename)
	if err != nil {
		return cfg, err
	}

	if err := json.NewDecoder(file).Decode(&cfg); err != nil {
		return cfg, err
	}

	// switch cfg.Period {
	// case "weekly":
	// 	cfg.PeriodType = hledger.PeriodWeekly
	// case "monthly":
	// 	cfg.PeriodType = hledger.PeriodMonthly
	// case "quarterly":
	// 	cfg.PeriodType = hledger.PeriodQuarterly
	// case "yearly":
	// 	cfg.PeriodType = hledger.PeriodYearly
	// default:
	// 	cfg.PeriodType = hledger.PeriodYearly
	// }
	//
	// if cfg.Reports == nil || len(cfg.Reports) == 0 {
	// 	cfg.Reports = defaultReports()
	// }
	return cfg, nil
}
