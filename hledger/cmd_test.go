package hledger

import "testing"

func TestBuildCommand(t *testing.T) {
	af := NewAccountFilter("dbs")
	df := NewDateFilter().LastMonth()

	tests := map[string]struct {
		hledgerCmd string
		expected   string
	}{
		"balance": {
			hledgerCmd: "balance",
			expected:   `hledger balance acct:dbs date:"last month" -O csv`,
		},
		"register": {
			hledgerCmd: "register",
			expected:   `hledger register acct:dbs date:"last month" -O csv`,
		},
	}

	for name, test := range tests {
		t.Run(name, func(t *testing.T) {
			command := buildCmd(test.hledgerCmd, af, df)
			if command != test.expected {
				t.Errorf("expected=%s, got=%s", test.expected, command)
			}
		})
	}
}
