package hledger

import (
	"fmt"
	"os/exec"
	"strings"
)

type Hledger struct {
}

func New() Hledger {
	return Hledger{}
}

func (h *Hledger) Register(filters ...Filter) ([]Transaction, error) {
	command := h.buildRegisterCommand(filters...)
	out, err := execute(command, true)
	if err != nil {
		return nil, fmt.Errorf("%s: %w", string(out), err)
	}

	// TODO use io.Reader from the command directly instead of doing this
	return parseFromCSV(strings.NewReader(string(out))), nil
}

func (h *Hledger) Balance() (string, error) {
	return "", fmt.Errorf("not implemented")
}

func (h *Hledger) buildRegisterCommand(filters ...Filter) string {
	base := "hledger print "
	for _, f := range filters {
		base += f.Build()
	}
	return base + `-p "last month"`
}

func execute(command string, isCSV bool) ([]byte, error) {
	if isCSV {
		command += ` -O csv`
	}

	return exec.Command("bash", "-c", command).CombinedOutput()
}
