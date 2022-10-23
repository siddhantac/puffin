package hledger

import "testing"

func TestBuildRegisterCommand(t *testing.T) {
	h := Hledger{}
	command := h.buildRegisterCommand()
	expected := `hledger print dbs -p "last month"`

	if command != expected {
		t.Errorf("expected=%s, got=%s", expected, command)
	}
}
