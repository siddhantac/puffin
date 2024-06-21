package accounting

import (
	"encoding/csv"
	"errors"
	"io"
	"log"

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
		rows    []table.Row
		columns table.Row
	}
)

func (rd RegisterData) Columns() table.Row { return rd.columns }
func (rd RegisterData) Rows() []table.Row  { return rd.rows }

type MsgError string

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
		log.Printf("error: %v", err.Error())
	} else {
		log.Printf("msg: %v, err: %v, args: %v", e.Msg(), e.Error(), e.Args())
	}
	return MsgError(e.Msg())
}

func (c HledgerCmd) Register(options hledger.Options) tea.Cmd {
	return func() tea.Msg {
		log.Printf("options: %v", options.Build())
		data, err := c.hldg.Register(options)
		if err != nil {
			return handleHledgerError(err)
		}
		records, err := parseCSV(data)
		if err != nil {
			log.Printf("parse csv: %s", err.Error())
			return MsgError(err.Error())
		}
		log.Printf("register data: %d records", len(records))
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
			return MsgError(err.Error())
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
			return MsgError(err.Error())
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
			return MsgError(err.Error())
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
			return MsgError(err.Error())
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
			return MsgError(err.Error())
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
			return MsgError(err.Error())
		}
		return ExpensesData(b)
	}
}
