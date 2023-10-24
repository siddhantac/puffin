package ui

import (
	"io"
	"puffin/hledger"

	"github.com/charmbracelet/bubbles/table"
	tea "github.com/charmbracelet/bubbletea"
)

type HledgerCmd struct {
	hl hledger.Hledger
}

func NewHledgerCmd(hl hledger.Hledger) HledgerCmd {
	return HledgerCmd{hl: hl}
}

type assetsData string
type incomeStatementData string
type balanceSheetData string
type expensesData string
type revenueData string
type liabilitiesData string

type transactionsData []table.Row

type msgError struct {
	err error
}

func (m msgError) Error() string { return m.err.Error() }

func (c HledgerCmd) register(isReversed bool, filter ...hledger.Filter) tea.Cmd {
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

type hlcmd func(...hledger.Filter) (io.Reader, error)

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
