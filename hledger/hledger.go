package hledger

import (
	"fmt"
	"os/exec"
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
		return nil, err
	}

	return out, nil
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

func execute(command string, isCSV bool) ([]Transaction, error) {
	if isCSV {
		command += ` -O csv`
	}

	cmd := exec.Command("bash", "-c", command)

	stdout, err := cmd.StdoutPipe()
	if err != nil {
		return nil, err
	}

	if err := cmd.Start(); err != nil {
		return nil, err
	}

	data := parseFromCSV(stdout)
	if err := cmd.Wait(); err != nil {
		return data, err
	}

	return data, nil
}
