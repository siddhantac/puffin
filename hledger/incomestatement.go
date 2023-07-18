package hledger

import "fmt"

func (h Hledger) IncomeStatement(filters ...Filter) ([][]string, error) {
	rd, err := execCmd("incomestatement", true, filters...)
	if err != nil {
		return nil, err
	}

	data, err := parseCSV(rd, 1)
	if err != nil {
		return nil, fmt.Errorf("income statement: %w", err)
	}
	return data, nil
}
