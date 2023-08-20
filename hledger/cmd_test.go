package hledger

import (
	"fmt"
	"testing"
)

func TestBuildCommand(t *testing.T) {
	af := NewAccountFilter("dbs")
	df := NewDateFilter().LastMonth()

	tests := map[string]struct {
		hledgerCmd []string
		expected   []string
	}{
		"balance": {
			hledgerCmd: []string{"balance"},
			expected: []string{
				"balance",
				"acct:dbs",
				"date:\"last month\"",
				"-O",
				"csv",
			},
		},
		"register": {
			hledgerCmd: []string{"register"},
			expected: []string{
				"register",
				"acct:dbs",
				"date:\"last month\"",
				"-O",
				"csv",
			},
		},
		"incomestatement": {
			hledgerCmd: []string{"incomestatement"},
			expected: []string{
				"incomestatement",
				"acct:dbs",
				"date:\"last month\"",
				"-O",
				"csv",
			},
		},
	}

	for name, test := range tests {
		t.Run(name, func(t *testing.T) {
			var h Hledger
			command := h.buildCmd(test.hledgerCmd, af, df)
			if err := compareSlice(test.expected, command); err != nil {
				t.Errorf("%v\n\twant=%v, got=%v", err, test.expected, command)
			}
		})
	}
}

func compareSlice(want, got []string) error {
	if len(want) != len(got) {
		return fmt.Errorf("unequal length: want=%d, got=%d", len(want), len(got))
	}
	return nil
}
