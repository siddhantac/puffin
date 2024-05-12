package accounting

import (
	"encoding/csv"
	"errors"
	"fmt"
	"io"
	"strconv"

	tea "github.com/charmbracelet/bubbletea"
	"github.com/siddhantac/hledger"
)

type IncomeStatementChartData struct {
	Revenue, Expenses float64
}

func (c HledgerCmd) IncomestatementValues(options hledger.Options) tea.Cmd {
	return func() tea.Msg {
		data, err := c.hldg.IncomeStatement(options)
		if err != nil {
			return handleHledgerError(err)
		}

		var iscd IncomeStatementChartData
		var flag bool
		csvrdr := csv.NewReader(data)
		for {
			rec, err := csvrdr.Read()
			if errors.Is(err, io.EOF) {
				break
			}
			if err != nil {
				return MsgError(fmt.Errorf("error reading csv: %w: %s", err, rec).Error())
			}

			if rec[0] == "total" && !flag {
				flag = true
				iscd.Revenue, err = strconv.ParseFloat(rec[2], 32)
				if err != nil {
					return MsgError(fmt.Errorf("error parsing revenue: %w", err).Error())
				}
			}

			if rec[0] == "total" && flag {
				iscd.Expenses, err = strconv.ParseFloat(rec[2], 32)
				if err != nil {
					return MsgError(fmt.Errorf("error parsing expenses: %w", err).Error())
				}
			}
		}

		return iscd
	}
}
