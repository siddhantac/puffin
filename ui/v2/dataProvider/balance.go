package dataprovider

import (
	"encoding/csv"
	"errors"
	"fmt"
	"io"
	"log"
	"os/exec"
)

func runCommand(args []string) (io.Reader, error) {
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

func parseCSV(r io.Reader) ([][]string, error) {
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
		result = append(result, rec)
	}
	return result, nil
}

func AccountBalances() ([][]string, error) {
	args := []string{"balance", "--depth=1", "-p", "2024", "-O", "csv"}
	r, err := runCommand(args)
	if err != nil {
		return nil, fmt.Errorf("failed to run command: %w", err)
	}

	rows, err := parseCSV(r)
	if err != nil {
		return nil, fmt.Errorf("failed to parse csv: %w", err)
	}

	return rows, nil
}

func SubAccountBalances(account string) ([][]string, error) {
	log.Printf("data: account: %s", account)
	args := []string{"balance", account, "--drop=1", "-p", "2024", "-O", "csv"}
	r, err := runCommand(args)
	if err != nil {
		return nil, fmt.Errorf("failed to run command: %w", err)
	}

	rows, err := parseCSV(r)
	if err != nil {
		return nil, fmt.Errorf("failed to parse csv: %w", err)
	}

	return rows, nil
}
