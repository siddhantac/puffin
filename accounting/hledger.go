package accounting

import (
	"encoding/csv"
	"errors"
	"io"
	"puffin/logger"

	"github.com/charmbracelet/bubbles/table"
	tea "github.com/charmbracelet/bubbletea"
	"github.com/siddhantac/hledger"
)

type HledgerCmd struct {
	hldg hledger.Hledger
}

func NewHledgerCmd(hldg hledger.Hledger) HledgerCmd {
	return HledgerCmd{hldg: hldg}
}

type (
	AssetsData          string
	IncomeStatementData string
	BalanceSheetData    string
	ExpensesData        string
	RevenueData         string
	LiabilitiesData     string

	RegisterData struct {
		Rows    []table.Row
		Columns table.Row
	}
)

type MsgError struct {
	err error
}

func (m MsgError) Error() string { return m.err.Error() }

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

func handleHledgerError(err error) MsgError {
	e, ok := err.(*hledger.Error)
	if !ok {
		logger.Logf("register: %v", err.Error())
	} else {
		logger.Logf("register: %v, %v", e.Error(), e.Msg())
	}
	return MsgError{err}
}

func (c HledgerCmd) Register(options hledger.Options) tea.Cmd {
	return func() tea.Msg {
		logger.Logf("options: %v", options.Build())
		data, err := c.hldg.Register(options)
		if err != nil {
			return handleHledgerError(err)
		}
		records, err := parseCSV(data)
		if err != nil {
			logger.Logf("parse csv: %s", err.Error())
			return MsgError{err}
		}
		return CreateRegisterData(records)
	}
}

// TODO: rename 'c' to something better
func (c HledgerCmd) Assets(options hledger.Options) tea.Cmd {
	return func() tea.Msg {
		data, err := c.hldg.Assets(options)
		if err != nil {
			return handleHledgerError(err)
		}
		b, err := io.ReadAll(data)
		if err != nil {
			return MsgError{err}
		}
		return AssetsData(b)
	}
}

func (c HledgerCmd) Revenue(options hledger.Options) tea.Cmd {
	return func() tea.Msg {
		data, err := c.hldg.Revenue(options)
		if err != nil {
			return handleHledgerError(err)
		}
		b, err := io.ReadAll(data)
		if err != nil {
			return MsgError{err}
		}
		return RevenueData(b)
	}
}

func (c HledgerCmd) Liabilities(options hledger.Options) tea.Cmd {
	return func() tea.Msg {
		data, err := c.hldg.Liabilities(options)
		if err != nil {
			return handleHledgerError(err)
		}
		b, err := io.ReadAll(data)
		if err != nil {
			return MsgError{err}
		}
		return LiabilitiesData(b)
	}
}

func (c HledgerCmd) Incomestatement(options hledger.Options) tea.Cmd {
	return func() tea.Msg {
		data, err := c.hldg.IncomeStatement(options)
		if err != nil {
			return handleHledgerError(err)
		}
		b, err := io.ReadAll(data)
		if err != nil {
			return MsgError{err}
		}
		return IncomeStatementData(b)
	}
}

func (c HledgerCmd) Balancesheet(options hledger.Options) tea.Cmd {
	return func() tea.Msg {
		data, err := c.hldg.BalanceSheet(options)
		if err != nil {
			return handleHledgerError(err)
		}
		b, err := io.ReadAll(data)
		if err != nil {
			return MsgError{err}
		}
		return BalanceSheetData(b)
	}
}

func (c HledgerCmd) Expenses(options hledger.Options) tea.Cmd {
	return func() tea.Msg {
		data, err := c.hldg.Expenses(options)
		if err != nil {
			return handleHledgerError(err)
		}
		b, err := io.ReadAll(data)
		if err != nil {
			return MsgError{err}
		}
		return ExpensesData(b)
	}
}
