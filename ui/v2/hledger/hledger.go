package hledger

import (
	"encoding/csv"
	"errors"
	"fmt"
	"io"
	"log"
	"os/exec"
	"slices"
	"strconv"

	"github.com/siddhantac/puffin/ui/v2/interfaces"
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

func (hd HledgerData) Balance(filter interfaces.Filter, displayOptions interfaces.DisplayOptions) ([][]string, error) {
	args := []string{"balance", filter.AccountType, "--layout=bare", "-O", "csv"}
	filters := prepareFilters(filter.Account, filter.DateStart, filter.DateEnd, "")
	args = append(args, filters...)

	options := argsFromDisplayOptions(displayOptions)
	args = append(args, options...)

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

func (hd HledgerData) Records(filter interfaces.Filter) ([][]string, error) {
	args := []string{"aregister", "-O", "csv"}
	filters := prepareFilters(filter.Account, filter.DateStart, filter.DateEnd, filter.Description)
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

func (hd HledgerData) IncomeStatement(filter interfaces.Filter, displayOptions interfaces.DisplayOptions) (*interfaces.ComplexTable, error) {
	args := []string{"incomestatement", "--pretty", "-O", "csv", "--layout", "bare"}
	filters := prepareFilters(filter.Account, filter.DateStart, filter.DateEnd, "")
	args = append(args, filters...)

	options := argsFromDisplayOptions(displayOptions)
	args = append(args, options...)

	r, err := hd.runCommand(args)
	if err != nil {
		return nil, fmt.Errorf("failed to run command: %w", err)
	}

	ct, err := hd.csvToComplexTable(r)
	if err != nil {
		return nil, fmt.Errorf("failed to convert csv to complexTable: %w", err)
	}

	return ct, nil
}

func (hd HledgerData) BalanceSheet(filter interfaces.Filter, displayOptions interfaces.DisplayOptions) (*interfaces.ComplexTable, error) {
	args := []string{"balancesheet", "--pretty", "-O", "csv", "--layout", "bare"}
	filters := prepareFilters(filter.Account, filter.DateStart, filter.DateEnd, "")
	args = append(args, filters...)

	options := argsFromDisplayOptions(displayOptions)
	args = append(args, options...)

	r, err := hd.runCommand(args)
	if err != nil {
		return nil, fmt.Errorf("failed to run command: %w", err)
	}

	ct, err := hd.csvToComplexTable(r)
	if err != nil {
		return nil, fmt.Errorf("failed to convert csv to complexTable: %w", err)
	}

	return ct, nil
}

func (hd HledgerData) csvToComplexTable(r io.Reader) (*interfaces.ComplexTable, error) {
	ct := new(interfaces.ComplexTable)
	rows, err := hd.parseCSV(r)
	if err != nil {
		return nil, fmt.Errorf("failed to parse csv: %w", err)
	}

	ct.Title = rows[0][0]
	ct.Columns = rows[1]
	ct.Upper = make([][]string, 0)
	ct.Lower = make([][]string, 0)

	index := 0
	ct.UpperTitle = rows[2][0]
	for i, row := range rows[3:] {
		if row[0] == "Expenses" || row[0] == "Liabilities" {
			index = i + 3 // 4 to offset because we started iterating from row 3
			break
		}
		ct.Upper = append(ct.Upper, row)
	}

	ct.LowerTitle = rows[index][0]
	for _, row := range rows[index+1 : len(rows)-1] { // start from index+1 to skip row 'Expenses'
		ct.Lower = append(ct.Lower, row)
		if row[0] == "Total:" {
			break
		}
	}

	for i := len(rows) - 1; i >= 0; i-- {
		row := rows[i]
		if row[0] != "Net:" {
			break
		}
		ct.BottomBar = append(ct.BottomBar, row)
	}
	slices.Reverse(ct.BottomBar)

	return ct, nil
}

func argsFromDisplayOptions(displayOptions interfaces.DisplayOptions) []string {
	result := []string{"--depth", strconv.Itoa(displayOptions.Depth)}

	switch displayOptions.Interval {
	case interfaces.Yearly:
		result = append(result, "--yearly")
	case interfaces.Monthly:
		result = append(result, "--monthly")
		// case "quarterly":
		// 	result = append(result, "--quarterly")
		// case "weekly":
		// 	result = append(result, "--weekly")
	}

	switch displayOptions.Sort {
	case interfaces.ByAmount:
		result = append(result, "--sort")
	}

	return result
}

func prepareFilters(account, from, to, description string) []string {
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
	if description != "" {
		args = append(args, "desc:"+description)
	}
	return args
}

func columnSelector(in []string) []string {
	return []string{in[1], in[3], in[4], in[5]}
}

type modifier func([]string) []string
