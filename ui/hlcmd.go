package ui

import (
	"encoding/csv"
	"errors"
	"fmt"
	"hledger/hledger"
	"io"

	"github.com/charmbracelet/bubbles/table"
	tea "github.com/charmbracelet/bubbletea"
)

type HledgerCmd struct {
	hl hledger.Hledger
}

func NewHledgerCmd(hl hledger.Hledger) HledgerCmd {
	return HledgerCmd{hl: hl}
}

type tableData struct {
	rows    []table.Row
	columns []table.Column
}

type msgError struct {
	err error
}

func (m msgError) Error() string { return m.err.Error() }

func (c *HledgerCmd) runMyCommand(account string) tea.Cmd {
	return func() tea.Msg {
		acctFilter := hledger.NewAccountFilter(account)
		data, err := c.hl.Register(acctFilter)
		if err != nil {
			return msgError{err}
		}

		return toRows(data)
	}
}

func toRows(txns []hledger.Transaction) tableData {
	td := tableData{
		rows: make([]table.Row, 0),
	}

	for _, txn := range txns {
		row := []string{
			txn.ID,
			txn.Date,
			txn.Description,
			txn.FromAccount,
			txn.Amount,
		}

		td.rows = append(td.rows, row)
	}

	return td
}

func parseData(data io.Reader) tableData {
	csvReader := csv.NewReader(data)

	td := tableData{
		rows:    make([]table.Row, 0),
		columns: make([]table.Column, 0),
	}

	_, _ = csvReader.Read() // ignore the first row

	for {
		record, err := csvReader.Read()
		if errors.Is(err, io.EOF) {
			break
		}

		if err != nil {
			fmt.Println("error:", err)
			break
		}

		modRecord := []string{}
		modRecord = append(modRecord, record[0], record[1], record[5], record[7], record[8])
		td.rows = append(td.rows, modRecord)
	}

	return td
}
