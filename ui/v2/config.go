package ui

import (
	"encoding/json"
	"io"
	"os"
)

type Config struct {
	JournalFile string `json:"journalFile"`
}

func NewConfig(configFile string) (Config, error) {
	file, err := os.Open(configFile)
	if err != nil {
		return Config{}, err
	}

	return parseJSON(file)
}

func parseJSON(file io.Reader) (Config, error) {
	var cfg Config
	if err := json.NewDecoder(file).Decode(&cfg); err != nil {
		return cfg, err
	}

	return cfg, nil
}
