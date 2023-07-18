package hledger

import (
	"strings"
)

func (h Hledger) Register(filters ...Filter) ([][]string, error) {
	rd, err := execCmd("register", true, filters...)
	if err != nil {
		return nil, err
	}

	data := parseCSV(rd, 0)
	return data, nil
}

func shortAccountName(s string) string {
	s = strings.Replace(s, "liabilities", "lia", 1)
	s = strings.Replace(s, "expenses", "exp", 1)
	s = strings.Replace(s, "credit_card", "cc", 1)
	s = strings.Replace(s, "income", "inc", 1)
	s = strings.Replace(s, "assets", "ast", 1)
	return s
}
