package hledger

import "io"

func (h Hledger) BalanceSheet(filters ...Filter) (io.Reader, error) {
	args := []string{"balancesheet", "--pretty", "--drop", "1", "-S", "--layout", "bare"}
	rd, err := h.execWithoutCSV(args, filters...)
	if err != nil {
		return nil, err
	}
	return rd, nil
}
