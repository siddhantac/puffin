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
	hldg hlgo.Hledger
}

func NewHledgerCmd(hldg hlgo.Hledger) HledgerCmd {
	return HledgerCmd{hldg: hldg}
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
		data, err := c.hldg.Register(options)
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

// TODO: rename this to something better
type hlcmd func(...hledger.Filter) (io.Reader, error)

// TODO: rename 'c' to something better
func (c HledgerCmd) assets(options hlgo.Options) tea.Cmd {
	return func() tea.Msg {
		data, err := c.hldg.Assets(options)
		if err != nil {
			return handleHledgerError(err)
		}
		b, err := io.ReadAll(data)
		if err != nil {
			return msgError{err}
		}
		return assetsData(b)
	}
}

func (c HledgerCmd) revenue(options hlgo.Options) tea.Cmd {
	return func() tea.Msg {
		data, err := c.hldg.Revenue(options)
		if err != nil {
			return handleHledgerError(err)
		}
		b, err := io.ReadAll(data)
		if err != nil {
			return msgError{err}
		}
		return revenueData(b)
	}
}

func (c HledgerCmd) liabilities(options hlgo.Options) tea.Cmd {
	return func() tea.Msg {
		data, err := c.hldg.Liabilities(options)
		if err != nil {
			return handleHledgerError(err)
		}
		b, err := io.ReadAll(data)
		if err != nil {
			return msgError{err}
		}
		return liabilitiesData(b)
	}
}

func (c HledgerCmd) incomestatement(options hlgo.Options) tea.Cmd {
	return func() tea.Msg {
		data, err := c.hldg.IncomeStatement(options)
		if err != nil {
			return handleHledgerError(err)
		}
		b, err := io.ReadAll(data)
		if err != nil {
			return msgError{err}
		}
		return incomeStatementData(b)
	}
}

func (c HledgerCmd) balancesheet(options hlgo.Options) tea.Cmd {
	return func() tea.Msg {
		data, err := c.hldg.BalanceSheet(options)
		if err != nil {
			return handleHledgerError(err)
		}
		b, err := io.ReadAll(data)
		if err != nil {
			return msgError{err}
		}
		return balanceSheetData(b)
	}
}

func (c HledgerCmd) expenses(options hlgo.Options) tea.Cmd {
	return func() tea.Msg {
		data, err := c.hldg.Expenses(options)
		if err != nil {
			return handleHledgerError(err)
		}
		b, err := io.ReadAll(data)
		if err != nil {
			return msgError{err}
		}
		return expensesData(b)
	}
}
