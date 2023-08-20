package hledger

import (
	"fmt"
	"io"
)

type ErrorMsg struct{ msg string }

func (e ErrorMsg) Error() string { return e.msg }

func (h Hledger) Balance(filters ...Filter) ([][]string, error) {
	rd, err := execCmd([]string{"balance"}, filters...)
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
