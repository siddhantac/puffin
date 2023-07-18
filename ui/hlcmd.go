package ui

import (
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
		// return transactionToRows(data, isReversed)
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

func (c HledgerCmd) incomestatement(filter ...hledger.Filter) tea.Cmd {
	return func() tea.Msg {
		data, err := c.hl.IncomeStatement(filter...)
		if err != nil {
			return msgError{err}
		}
		return createIncomeStatementData(data)
	}
}

func transactionToRows(txns []hledger.Transaction, isReversed bool) transactionsData {
	rows := make(transactionsData, len(txns))
	size := len(txns)

	for i := range txns {
		txn := txns[i]
		if isReversed {
			txn = txns[size-i-1]
		}
		row := []string{
			txn.ID,
			txn.Date,
			txn.Description,
			txn.FromAccount,
			txn.Amount,
		}

		rows[i] = row
	}

	return rows
}
