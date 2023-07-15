package ui

import (
	"puffin/hledger"
	"strings"

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
type incomeStatementData []table.Row

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

func (c HledgerCmd) incomestatement1(filter ...hledger.Filter) tea.Cmd {
	return func() tea.Msg {
		data, err := c.hl.IncomeStatement1(filter...)
		if err != nil {
			return msgError{err}
		}
		return incomeStatementToRows(data)
	}
}

func (c HledgerCmd) incomestatement2(filter ...hledger.Filter) tea.Cmd {
	return func() tea.Msg {
		data, err := c.hl.IncomeStatement2(filter...)
		if err != nil {
			return msgError{err}
		}
		return incomeStatementToRows(data)
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
			txn.FromAccount,
			txn.Amount,
		}

		rows[i] = row
	}

	return rows
}

func incomeStatementToRows(isData []hledger.IncomeStatement) incomeStatementData {
	rows := make(incomeStatementData, 0)

	for _, d := range isData {
		row := make([]string, 0)
		if strings.Contains(d.Name, ":") {
			d.Name = "\t" + d.Name
		}
		row = append(row, d.Name)
		row = append(row, d.Amounts...)
		rows = append(rows, row)
	}

	return rows
}
