package ui

import (
	"encoding/csv"
	"errors"
	"io"
	"puffin/hledger"
	"puffin/logger"

	"github.com/charmbracelet/bubbles/table"
	tea "github.com/charmbracelet/bubbletea"
	hlgo "github.com/siddhantac/hledger"
)

type HledgerCmd struct {
	hl  hledger.Hledger
	hl2 hlgo.Hledger
}

func NewHledgerCmd(hl hledger.Hledger, hl2 hlgo.Hledger) HledgerCmd {
	return HledgerCmd{hl: hl, hl2: hl2}
}

type (
	assetsData          string
	incomeStatementData string
	balanceSheetData    string
	expensesData        string
	revenueData         string
	liabilitiesData     string
)

type transactionsData []table.Row

type msgError struct {
	err error
}

func (m msgError) Error() string { return m.err.Error() }

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
			return nil, err
		}
		result = append(result, rec)
	}
	return result, nil
}

func handleHledgerError(err error) msgError {
	e, ok := err.(*hlgo.Error)
	if !ok {
		logger.Logf("register: %v", err.Error())
	} else {
		logger.Logf("register: %v, %v", e.Error(), e.Msg())
	}
	return msgError{err}
}

func (c HledgerCmd) register(options hlgo.Options) tea.Cmd {
	return func() tea.Msg {
		logger.Logf("options: %v", options.Build())
		data, err := c.hl2.Register(options)
		if err != nil {
			return handleHledgerError(err)
		}
		records, err := parseCSV(data)
		if err != nil {
			logger.Logf("parse csv: %s", err.Error())
			return msgError{err}
		}
		return createRegisterData(records)
	}
}

func (c HledgerCmd) register2(isReversed bool, filter ...hledger.Filter) tea.Cmd {
	return func() tea.Msg {
		data, err := c.hl.Register(filter...)
		if err != nil {
			return msgError{err}
		}
		return createRegisterData(data)
	}
}

func (c HledgerCmd) balance(filter ...hledger.Filter) tea.Cmd {
	return func() tea.Msg {
		data, err := c.hl.Balance(filter...)
		if err != nil {
			return msgError{err}
		}
		return createBalanceData(data)
	}
}

// TODO: rename this to something better
type hlcmd func(...hledger.Filter) (io.Reader, error)

// TODO: rename 'c' to something better
func processHlCmd(c hlcmd, filters ...hledger.Filter) ([]byte, error) {
	reader, err := c(filters...)
	if err != nil {
		return nil, err
	}
	b, err := io.ReadAll(reader)
	if err != nil {
		return nil, err
	}
	return b, nil
}

func (c HledgerCmd) assets(filter ...hledger.Filter) tea.Cmd {
	return func() tea.Msg {
		b, err := processHlCmd(c.hl.Assets, filter...)
		if err != nil {
			return msgError{err}
		}
		return assetsData(b)
	}
}

func (c HledgerCmd) revenue(filter ...hledger.Filter) tea.Cmd {
	return func() tea.Msg {
		b, err := processHlCmd(c.hl.Revenue, filter...)
		if err != nil {
			return msgError{err}
		}
		return revenueData(b)
	}
}

func (c HledgerCmd) liabilities(filter ...hledger.Filter) tea.Cmd {
	return func() tea.Msg {
		b, err := processHlCmd(c.hl.Liabilities, filter...)
		if err != nil {
			return msgError{err}
		}
		return liabilitiesData(b)
	}
}

func (c HledgerCmd) incomestatement(filter ...hledger.Filter) tea.Cmd {
	return func() tea.Msg {
		b, err := processHlCmd(c.hl.IncomeStatement, filter...)
		if err != nil {
			return msgError{err}
		}
		return incomeStatementData(b)
	}
}

func (c HledgerCmd) balancesheet(filter ...hledger.Filter) tea.Cmd {
	return func() tea.Msg {
		b, err := processHlCmd(c.hl.BalanceSheet, filter...)
		if err != nil {
			return msgError{err}
		}
		return balanceSheetData(b)
	}
}

func (c HledgerCmd) expenses(filter ...hledger.Filter) tea.Cmd {
	return func() tea.Msg {
		b, err := processHlCmd(c.hl.Expenses, filter...)
		if err != nil {
			return msgError{err}
		}
		return expensesData(b)
	}
}
