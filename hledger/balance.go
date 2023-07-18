package hledger

import "fmt"

func (h Hledger) Balance(filters ...Filter) ([][]string, error) {
	rd, err := execCmd("balance", true, filters...)
	if err != nil {
		return nil, err
	}

	data, err := parseCSV(rd, 0)
	if err != nil {
		return nil, fmt.Errorf("balance: %w", err)
	}
	return data, nil
}
