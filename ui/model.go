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

type model struct {
	table table.Model
	hl    hledger.Hledger
}

func newModel(hl hledger.Hledger) model {
	return model{hl: hl}
}

func (m model) Init() tea.Cmd { return nil }

func (m model) Update(msg tea.Msg) (tea.Model, tea.Cmd) {
	var cmd tea.Cmd
	switch msg := msg.(type) {
	case tea.KeyMsg:
		switch msg.String() {
		case "esc":
			if m.table.Focused() {
				m.table.Blur()
			} else {
				m.table.Focus()
			}
		case "q", "ctrl+c":
			return m, tea.Quit
		case "enter":
			return m, tea.Batch(
				tea.Printf("Let's go to %s!", m.table.SelectedRow()[1]),
			)
		case "f":
			m.table.SetRows(nil)

			acctFilter := hledger.NewAccountFilter("dbs_twisha")
			data, err := m.hl.Register(acctFilter)
			if err != nil {
				return m, tea.Printf("%v", err)
			}

			_, rows := m.parseData(data)
			m.table.SetRows(rows)

			return m, tea.Printf("you pressed f")
		}
	}
	m.table, cmd = m.table.Update(msg)
	return m, cmd
}

func (m model) View() string {
	return baseStyle.Render(m.table.View()) + "\n"
}

func (m model) getData() (io.Reader, error) {
	data, err := m.hl.Register()
	if err != nil {
		return nil, err
	}

	return data, nil
}

func (m model) parseData(data io.Reader) ([]table.Column, []table.Row) {
	csvReader := csv.NewReader(data)

	var columns []table.Column
	rows := make([]table.Row, 0)

	count := 0
	for {
		record, err := csvReader.Read()
		if errors.Is(err, io.EOF) {
			break
		}

		if err != nil {
			fmt.Println("error:", err)
			break
		}

		if count == 0 {
			count++
			columns = createHeader(record)
			continue
		}

		modRecord := []string{}
		modRecord = append(modRecord, record[0], record[1], record[5], record[7], record[8])
		rows = append(rows, modRecord)
	}

	return columns, rows
}
