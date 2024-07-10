package ui

import (
	"encoding/json"
	"fmt"
	"os"
	"time"

	"github.com/siddhantac/hledger"
)

type Report struct {
	Cmd           string   `json:"cmd"`
	Args          []string `json:"args"`
	Name          string   `json:"name"`
	Locked bool     `json:"ignoreOptions"`
}

type Config struct {
	PeriodType hledger.PeriodType `json:"-"`
	Period     string             `json:"period"`
	StartDate  string             `json:"startDate"`
	EndDate    string             `json:"endDate"`
	Reports    []Report           `json:"reports"`
}

// func (c *Config) UnmarshalJSON(b []byte) error {
// 	var m map[string]string
// 	if err := json.Unmarshal(b, &m); err != nil {
// 		return err
// 	}
//
// 	pt, ok := m["period"]
// 	if ok {
// 		switch pt {
// 		case "weekly":
// 			c.PeriodType = hledger.PeriodWeekly
// 		case "monthly":
// 			c.PeriodType = hledger.PeriodMonthly
// 		case "quarterly":
// 			c.PeriodType = hledger.PeriodQuarterly
// 		case "yearly":
// 			c.PeriodType = hledger.PeriodYearly
// 		default:
// 			c.PeriodType = hledger.PeriodYearly
// 		}
// 	}
//
// 	c.StartDate = m["startDate"]
// 	c.EndDate = m["endDate"]
// 	return nil
// }

func NewConfig(filename string) (Config, error) {
	var cfg Config

	file, err := os.Open(filename)
	if err != nil {
		return cfg, err
	}

	if err := json.NewDecoder(file).Decode(&cfg); err != nil {
		return cfg, err
	}

	switch cfg.Period {
	case "weekly":
		cfg.PeriodType = hledger.PeriodWeekly
	case "monthly":
		cfg.PeriodType = hledger.PeriodMonthly
	case "quarterly":
		cfg.PeriodType = hledger.PeriodQuarterly
	case "yearly":
		cfg.PeriodType = hledger.PeriodYearly
	default:
		cfg.PeriodType = hledger.PeriodYearly
	}
	return cfg, nil
}

var DefaultConfig = Config{
	PeriodType: hledger.PeriodYearly,
	StartDate:  fmt.Sprintf("%v", time.Now().Year()),
	EndDate:    "",
}
