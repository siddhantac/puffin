package ui

import (
	"bytes"
	"log"
	"strings"
	"time"

	"github.com/charmbracelet/bubbles/table"
	tea "github.com/charmbracelet/bubbletea"
	"github.com/siddhantac/hledger"
)

// incomeStatementView renders the Income Statement CSV as text, enriched with Q1, Q2, and YTD columns.
// It reuses the base command and options, and on content load it computes additional period totals
// by issuing constrained incomestatement commands for Q1 (Jan-Mar), Q2 (Apr-Jun), and YTD (Jan-today).
// The output is a CSV string shown in a viewport (similar to pager).

type incomeStatementView struct {
	id          int
	locked      bool
	cmd         func(int, hledger.Options) content
	cmdType     cmdType
	name        string

	table       *Table
	isDataReady bool
	lastWS      tea.WindowSizeMsg

	lastOptions hledger.Options // captured on Run
}

func newIncomeStatementView(id int, name string, locked bool, cmd func(int, hledger.Options) content) *incomeStatementView {
	return &incomeStatementView{
		id:      id,
		name:    name,
		locked:  locked,
		cmd:     cmd,
		cmdType: cmdUnknown,
	}
}

func (v *incomeStatementView) log(msg string) { log.Printf("%s(%d): %s", v.name, v.id, msg) }

func (v *incomeStatementView) SetContent(gc content) {
	if gc.id != v.id { return }

	rows, err := parseCSV(strings.NewReader(gc.msg))
	if err != nil || len(rows) < 2 {
		// Fall back to raw content
		v.table = newTable(v.name, nil, v.id, v.cmd, v.locked, cmdUnknown, []dataTransformer{highlightISHeaders{}})
		v.table.SetContent(gc)
		v.isDataReady = v.table.IsReady()
		return
	}

	// Enrich and render via Table
	enriched := v.enrichIncomeStatementRows(rows)
	csvText := rowsToCSV(enriched)
	if v.table == nil {
		v.table = newTable(v.name, nil, v.id, v.cmd, v.locked, cmdUnknown, []dataTransformer{highlightISHeaders{}})
	}
	v.table.SetContent(content{id: v.id, msg: csvText})
	// If we have a stored window size, apply it so columns compute widths
	if v.lastWS.Width > 0 && v.lastWS.Height > 0 {
		_, _ = v.table.Update(v.lastWS)
	}
	v.isDataReady = v.table.IsReady()
	v.log("ready")
}

func (v *incomeStatementView) IsReady() bool { return v.isDataReady }
func (v *incomeStatementView) Type() cmdType { return v.cmdType }
func (v *incomeStatementView) Locked() bool  { return v.locked }
func (v *incomeStatementView) SetUnready()   { v.isDataReady = false; v.log("unready") }

func (v *incomeStatementView) Run(options hledger.Options) tea.Cmd {
	// capture options so we can issue additional queries for Q1/Q2/YTD
	v.lastOptions = options
	return func() tea.Msg { return v.cmd(v.id, options) }
}

func (v *incomeStatementView) Init() tea.Cmd { v.SetUnready(); return nil }

func (v *incomeStatementView) Update(msg tea.Msg) (tea.Model, tea.Cmd) {
	switch m := msg.(type) {
	case tea.WindowSizeMsg:
		v.lastWS = m
	}
	if v.table == nil {
		return v, nil
	}
	var cmd tea.Cmd
	_, cmd = v.table.Update(msg)
	return v, cmd
}

func (v *incomeStatementView) View() string {
	if v.table == nil { return "" }
	return v.table.View()
}

// enrichIncomeStatementRows appends Q1, Q2, and YTD columns to the CSV rows.
// It obtains the three period datasets via hledger incomestatement calls with constrained dates,
// then merges the totals by account into the base rows.
func (v *incomeStatementView) enrichIncomeStatementRows(base []table.Row) []table.Row {
	if len(base) < 2 {
		return base
	}

	// Build Q1, Q2, YTD date ranges for current calendar year
	now := time.Now()
	year := now.Year()
	q1Start := time.Date(year, time.January, 1, 0, 0, 0, 0, time.Local)
	q1EndEx := time.Date(year, time.April, 1, 0, 0, 0, 0, time.Local)
	q2Start := time.Date(year, time.April, 1, 0, 0, 0, 0, time.Local)
	q2EndEx := time.Date(year, time.July, 1, 0, 0, 0, 0, time.Local)
	ytdStart := q1Start
	ytdEndEx := now.AddDate(0, 0, 1) // include today

	// Fetch period data; force CSV; use yearly period to get a single column we can sum easily
	get := func(from, to time.Time) []table.Row {
		opts := v.lastOptions.
			WithStartDate(from.Format("2006-01-02")).
			WithEndDate(to.Format("2006-01-02")).
			WithPeriod(hledger.PeriodYearly).
			WithOutputCSV(true)
		c := v.cmd(v.id, opts)
		rows, err := parseCSV(strings.NewReader(c.msg))
		if err != nil {
			return nil
		}
		return rows
	}

	q1Rows := get(q1Start, q1EndEx)
	q2Rows := get(q2Start, q2EndEx)
	ytdRows := get(ytdStart, ytdEndEx)

	aggQ1 := makeISPeriodLookup(q1Rows)
	aggQ2 := makeISPeriodLookup(q2Rows)
	aggYTD := makeISPeriodLookup(ytdRows)

	// Prepare output by copying base and appending empty cells
	// Build output skipping the title row (base[0]) so header is first
	out := make([]table.Row, 0, len(base)-1)
	for i, row := range base {
		if i == 0 { // skip title line
			continue
		}
		if i == 1 {
			// Header row: append new column titles
			head := append(append([]string{}, row...), "Q1", "Q2", "YTD")
			out = append(out, head)
			continue
		}
		// Other rows: append placeholders; we'll fill as we go
		newRow := append([]string{}, row...)
		newRow = append(newRow, "", "", "")
		out = append(out, newRow)
	}

	// Merge values by scanning base rows and filling from lookups
	mode := ""
	for i := 2; i < len(base); i++ { // start after title and header
		outIdx := i - 1 // because we skipped base[0]
		name := strings.ToLower(strings.TrimSpace(base[i][0]))
		if name == "revenues" || name == "revenue" {
			mode = "upper"
			continue
		}
		if name == "expenses" {
			mode = "lower"
			continue
		}
		if name == "net:" || name == "net" {
			// Fill net row from aggregated period nets
			fillRow(out[outIdx], aggQ1.net, aggQ2.net, aggYTD.net)
			continue
		}
		if name == "total:" || name == "total" {
			if mode == "upper" {
				fillRow(out[outIdx], aggQ1.totalUpper, aggQ2.totalUpper, aggYTD.totalUpper)
			} else if mode == "lower" {
				fillRow(out[outIdx], aggQ1.totalLower, aggQ2.totalLower, aggYTD.totalLower)
			}
			continue
		}
		if mode == "upper" {
			fillRow(out[outIdx], aggQ1.upper[name], aggQ2.upper[name], aggYTD.upper[name])
		} else if mode == "lower" {
			fillRow(out[outIdx], aggQ1.lower[name], aggQ2.lower[name], aggYTD.lower[name])
		}
	}

	return out
}

// isPeriodLookup holds aggregated values for a single period
// upper/lower maps: account name (lowercased) -> formatted string amount
// totals and net are formatted strings

type isPeriodLookup struct {
	upper      map[string]string
	lower      map[string]string
	totalUpper string
	totalLower string
	net        string
}

func makeISPeriodLookup(rows []table.Row) isPeriodLookup {
	pl := isPeriodLookup{
		upper: map[string]string{},
		lower: map[string]string{},
	}
	if len(rows) < 2 {
		return pl
	}

	mode := ""
	for i := 2; i < len(rows); i++ {
		if len(rows[i]) == 0 { continue }
		name := strings.ToLower(strings.TrimSpace(rows[i][0]))
		if name == "revenues" || name == "revenue" { mode = "upper"; continue }
		if name == "expenses" { mode = "lower"; continue }
		if name == "net:" || name == "net" {
			pl.net = formatCurrency(detectCurrencyPrefixFromRow(rows[i]), sumRowValues(rows[i]))
			continue
		}
		if name == "total:" || name == "total" {
			if mode == "upper" {
				pl.totalUpper = formatCurrency(detectCurrencyPrefixFromRow(rows[i]), sumRowValues(rows[i]))
			} else if mode == "lower" {
				pl.totalLower = formatCurrency(detectCurrencyPrefixFromRow(rows[i]), sumRowValues(rows[i]))
			}
			continue
		}
		if mode == "upper" {
			pl.upper[name] = formatCurrency(detectCurrencyPrefixFromRow(rows[i]), sumRowValues(rows[i]))
		} else if mode == "lower" {
			pl.lower[name] = formatCurrency(detectCurrencyPrefixFromRow(rows[i]), sumRowValues(rows[i]))
		}
	}
	return pl
}

// sumRowValues sums numeric values across all value columns (cells after the first)
func sumRowValues(row table.Row) float64 {
	var sum float64
	for j := 1; j < len(row); j++ {
		sum += parseAmount(row[j])
	}
	return sum
}

func detectCurrencyPrefixFromRow(row table.Row) string {
	for j := 1; j < len(row); j++ {
		if strings.TrimSpace(row[j]) != "" {
			return detectCurrencyPrefix(row[j])
		}
	}
	return ""
}

func fillRow(row table.Row, a, b, c string) {
	if len(row) < 4 { return }
	row[len(row)-3] = a
	row[len(row)-2] = b
	row[len(row)-1] = c
}

// rowsToCSV converts [][]string to a CSV string
func rowsToCSV(rows []table.Row) string {
	var buf bytes.Buffer
	w := newCSVWriter(&buf)
	for _, r := range rows {
		_ = w.Write(r)
	}
	w.Flush()
	return buf.String()
}

// newCSVWriter returns a csv.Writer configured with default comma
// Separated here to avoid importing encoding/csv at top-level unnecessarily in other files.
func newCSVWriter(buf *bytes.Buffer) *csvWriter { return &csvWriter{buf: buf} }

type csvWriter struct { buf *bytes.Buffer }

func (w *csvWriter) Write(record []string) error {
	// simple CSV writer with basic quoting for commas and quotes
	for i, s := range record {
		if i > 0 { w.buf.WriteByte(',') }
		needsQuote := strings.ContainsAny(s, ",\n\"")
		if needsQuote {
			w.buf.WriteByte('"')
			w.buf.WriteString(strings.ReplaceAll(s, "\"", "\"\""))
			w.buf.WriteByte('"')
		} else {
			w.buf.WriteString(s)
		}
	}
	w.buf.WriteByte('\n')
	return nil
}

func (w *csvWriter) Flush() {}

