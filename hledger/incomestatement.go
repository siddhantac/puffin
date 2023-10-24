package hledger

import (
	"fmt"
	"io"
)

func (h Hledger) IncomeStatementCSV(filters ...Filter) ([][]string, error) {
	args := []string{"incomestatement", "--drop", "1", "-S"}
	rd, err := h.execCmd(args, filters...)
	if err != nil {
		return nil, err
	}

	data, err := parseCSV(rd, 1)
	if err != nil {
		return nil, fmt.Errorf("income statement: %w", err)
	}
	return data, nil
}

func (h Hledger) IncomeStatement(filters ...Filter) (io.Reader, error) {
	args := []string{"incomestatement", "--pretty", "--drop", "1", "-S", "--layout", "bare"}
	rd, err := h.execWithoutCSV(args, filters...)
	if err != nil {
		return nil, err
	}
	return rd, nil
}
