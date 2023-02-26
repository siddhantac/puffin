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

type accountsData []table.Row
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
		return transactionToRows(data, isReversed)
	}
}

func (c HledgerCmd) balance(filter ...hledger.Filter) tea.Cmd {
	return func() tea.Msg {
		data, err := c.hl.Balance(filter...)
		if err != nil {
			return msgError{err}
		}
		return accountToRows(data)
	}
}

func accountToRows(accs []hledger.Account) accountsData {
	rows := make(accountsData, 0)

	for _, acc := range accs {
		row := []string{
			acc.Name,
			acc.Amount,
		}

		rows = append(rows, row)
	}

	return rows
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
			txn.AccountShortName,
			txn.Amount,
		}

		rows[i] = row
	}

	return rows
}
