package ui

import (
	"encoding/json"
	"fmt"
	"log"
	"os"
	"time"

	"github.com/siddhantac/hledger"
)

type Config struct {
	PeriodType hledger.PeriodType `json:"periodType"`
	StartDate  string             `json:"startDate"`
	EndDate    string             `json:"endDate"`
}

func (c Config) Unmarshal(b []byte) error {
	var m map[string]string
	if err := json.Unmarshal(b, &m); err != nil {
		return err
	}

	pt, ok := m["periodType"]
	if ok {
		switch pt {
		case "weekly":
			c.PeriodType = hledger.PeriodWeekly
		case "monthly":
			c.PeriodType = hledger.PeriodMonthly
		case "quarterly":
			c.PeriodType = hledger.PeriodQuarterly
		case "yearly":
			c.PeriodType = hledger.PeriodYearly
		default:
			c.PeriodType = hledger.PeriodYearly
		}
	}

	c.StartDate = m["startDate"]
	c.EndDate = m["endDate"]
	return nil
}

func NewConfig(filename string) (Config, error) {
	var cfg Config

	file, err := os.Open(filename)
	if err != nil {
		return cfg, err
	}

	if err := json.NewDecoder(file).Decode(&cfg); err != nil {
		return cfg, err
	}
	log.Printf("config: %v", cfg)
	return cfg, nil
}

var DefaultConfig = Config{
	PeriodType: hledger.PeriodYearly,
	StartDate:  fmt.Sprintf("%v", time.Now().Year()),
	EndDate:    "",
}
