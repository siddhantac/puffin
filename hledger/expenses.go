package hledger

import (
	"fmt"
	"io"
)

func (h Hledger) Expenses(filters ...Filter) (io.Reader, error) {
	d := NewDropAccountFilter()
	filters = append(filters, d)

	args := []string{"balance", "type:x", "--layout", "bare", "-S"}
	rd, err := h.execWithoutCSV(args, filters...)
	if err != nil {
		return nil, err
	}
	return rd, nil
}

func (h Hledger) ExpensesWithCSV(filters ...Filter) ([][]string, error) {
	d := NewDropAccountFilter()
	filters = append(filters, d)

	args := []string{"balance", "type:x", "--layout", "bare", "-S"}
	rd, err := h.execCmd(args, filters...)
	if err != nil {
		data, _ := io.ReadAll(rd)
		return nil, ErrorMsg{msg: string(data)}
	}

	data, err := parseCSV(rd, 0)
	if err != nil {
		return nil, fmt.Errorf("balance: %w", err)
	}
	return data, nil
}
