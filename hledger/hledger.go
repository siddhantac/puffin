package hledger

import (
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

func (h *Hledger) buildRegisterCommand(filters ...Filter) string {
	base := "hledger print "
	for _, f := range filters {
		base += f.Build()
	}
	return base + `-p "last month"`
}

func (h *Hledger) Balance(filters ...Filter) ([]Account, error) {
	command := h.buildBalanceCommand(filters...)
	out, err := execute2(command, true)
	if err != nil {
		return nil, err
	}

	return out, nil
}

func (h *Hledger) buildBalanceCommand(filters ...Filter) string {
	base := "hledger balance "
	for _, f := range filters {
		base += f.Build()
	}
	return base + `-p "last month"`
}

func execute(command string, isCSV bool) ([]Transaction, error) {
	aliases := ` --alias liabilities=lia --alias expenses=exp --alias income=in`
	command += aliases

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

	data := parseTransactionsFromCSV(stdout)
	if err := cmd.Wait(); err != nil {
		return data, err
	}

	return data, nil
}

func execute2(command string, isCSV bool) ([]Account, error) {
	aliases := ` --alias liabilities:credit_card=lia:cc --alias expenses=exp --alias income=in`
	command += aliases

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

	data := parseAccountsFromCSV(stdout)
	if err := cmd.Wait(); err != nil {
		return data, err
	}

	return data, nil
}
