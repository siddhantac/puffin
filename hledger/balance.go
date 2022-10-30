package hledger

import (
	"hledger/logger"
	"os/exec"
)

func (h Hledger) Balance(filters ...Filter) ([]Account, error) {
	command := h.buildBalanceCommand(filters...)
	out, err := executeBalance(command, true)
	if err != nil {
		return nil, err
	}

	return out, nil
}

func (h Hledger) buildBalanceCommand(filters ...Filter) string {
	base := "hledger balance "
	for _, f := range filters {
		base += f.Build()
	}
	return base
}

func executeBalance(command string, isCSV bool) ([]Account, error) {
	if isCSV {
		command += ` -O csv`
	}

	logger.Log("balance:" + command)
	cmd := exec.Command("bash", "-c", command)

	stdout, err := cmd.StdoutPipe()
	if err != nil {
		return nil, err
	}

	if err := cmd.Start(); err != nil {
		return nil, err
	}

	data := parseAccountsFromCSV(stdout)
	if err := cmd.Wait(); err != nil {
		return data, err
	}

	return data, nil
}
