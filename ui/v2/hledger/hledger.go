package hledger

import (
	"encoding/csv"
	"errors"
	"fmt"
	"io"
	"log"
	"os/exec"
)

type HledgerData struct {
}

func (hd HledgerData) runCommand(args []string) (io.Reader, error) {
	log.Printf("data: command: %v", args)
	cmd := exec.Command("hledger", args...)
	stdout, err := cmd.StdoutPipe()
	if err != nil {
		return nil, fmt.Errorf("failed to get stdout pipe: %w", err)
	}
	if err := cmd.Start(); err != nil {
		return nil, fmt.Errorf("failed to start command: %w", err)
	}

	go func() {
		if err := cmd.Wait(); err != nil {
			log.Fatal(err)
		}
	}()

	return stdout, err
}

func (hd HledgerData) parseCSV(r io.Reader, modifiers ...modifier) ([][]string, error) {
	result := make([][]string, 0)
	csvrdr := csv.NewReader(r)
	// csvrdr.Read() // skip 1 line
	for {
		rec, err := csvrdr.Read()
		if errors.Is(err, io.EOF) {
			break
		}
		if err != nil {
			return nil, fmt.Errorf("failed to read: %w", err)
		}
		for _, modify := range modifiers {
			rec = modify(rec)
		}
		result = append(result, rec)
	}
	return result, nil
}

func (hd HledgerData) AccountBalances() ([][]string, error) {
	args := []string{"balance", "--depth=1", "--layout=bare", "-p", "2025", "-O", "csv"}
	r, err := hd.runCommand(args)
	if err != nil {
		return nil, fmt.Errorf("failed to run command: %w", err)
	}

	rows, err := hd.parseCSV(r)
	if err != nil {
		return nil, fmt.Errorf("failed to parse csv: %w", err)
	}

	return rows, nil
}

func (hd HledgerData) SubAccountBalances(accountType, account, from, to string) ([][]string, error) {
	log.Printf("data: balance: accountType=%s, account=%s", accountType, account)
	args := []string{"balance", accountType, "--sort", "--layout=bare", "-O", "csv"}

	filters := prepareArgs(account, from, to)
	args = append(args, filters...)

	r, err := hd.runCommand(args)
	if err != nil {
		return nil, fmt.Errorf("failed to run command: %w", err)
	}

	rows, err := hd.parseCSV(r)
	if err != nil {
		return nil, fmt.Errorf("failed to parse csv: %w", err)
	}

	return rows, nil
}

func (hd HledgerData) Records(account, from, to string) ([][]string, error) {
	log.Printf("data: register: account=%s", account)
	args := []string{"register", account, "-O", "csv"}
	filters := prepareArgs(account, from, to)
	args = append(args, filters...)

	r, err := hd.runCommand(args)
	if err != nil {
		return nil, fmt.Errorf("failed to run command: %w", err)
	}

	rows, err := hd.parseCSV(r, columnSelector)
	if err != nil {
		return nil, fmt.Errorf("failed to parse csv: %w", err)
	}

	return rows, nil
}

func prepareArgs(account, from, to string) []string {
	args := []string{}
	if account != "" {
		args = append(args, account)
	}
	if from != "" {
		args = append(args, "-b", from)
	}
	if to != "" {
		args = append(args, "-e", to)
	}
	return args
}

func columnSelector(in []string) []string {
	return []string{in[1], in[3], in[4], in[5]}
}

type modifier func([]string) []string
