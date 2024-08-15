package accounting

import (
	"encoding/csv"
	"errors"
	"io"
	"log"

	"github.com/charmbracelet/bubbles/table"
	"github.com/siddhantac/hledger"
)

type HledgerCmd struct {
	hldg hledger.Hledger
}

func NewHledgerCmd(hldg hledger.Hledger) HledgerCmd {
	return HledgerCmd{hldg: hldg}
}

type (
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
