package hledger

import (
	"os/exec"
	"puffin/logger"
)

type Hledger struct {
}

func New() Hledger {
	return Hledger{}
}

func (h Hledger) Register(filters ...Filter) ([]Transaction, error) {
	command := h.buildRegisterCommand(filters...)
	out, err := executeRegister(command, true)
	if err != nil {
		return nil, err
	}

	return out, nil
}

func (h Hledger) buildRegisterCommand(filters ...Filter) string {
	base := "hledger register "
	for _, f := range filters {
		logger.Logf("building filter: %s", f.Name())
		base += f.Build()
	}
	return base
}

func executeRegister(command string, isCSV bool) ([]Transaction, error) {
	if isCSV {
		command += ` -O csv`
	}

	logger.Log("register:" + command)

	cmd := exec.Command("bash", "-c", command)

	stdout, err := cmd.StdoutPipe()
	if err != nil {
		return nil, err
	}

	if err := cmd.Start(); err != nil {
		return nil, err
	}

	data := parseCSVForReg(stdout)
	if err := cmd.Wait(); err != nil {
		return data, err
	}

	return data, nil
}
