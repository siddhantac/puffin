package hledger

import (
	"fmt"
	"io"
)

func (h Hledger) Expenses(filters ...Filter) ([][]string, error) {
	d := NewDropAccountFilter()
	filters = append(filters, d)

	rd, err := execCmd("balance type:x --layout bare", true, filters...)
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
