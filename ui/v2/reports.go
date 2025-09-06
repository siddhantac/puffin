package ui

import (
	"fmt"
	"log"
	"strings"
	"time"

	"github.com/siddhantac/puffin/ui/v2/interfaces"

	tea "github.com/charmbracelet/bubbletea"
	"github.com/charmbracelet/lipgloss"
)

type reports struct {
	incomeStatement     *complexTable
	balanceSheet        *complexTable
	dataProvider        interfaces.DataProvider
	filterGroup         *filterGroup
	displayOptionsGroup *displayOptionsGroup
	height, width       int
	cmdRunner           *cmdRunner

	// B/S charts view state (ui v2)
	showBSGraph   bool
	bsGraphReady  bool
	bsGraphString string
}

func newReports(dataProvider interfaces.DataProvider, cmdRunner *cmdRunner) *reports {
	a := &reports{
		dataProvider:        dataProvider,
		filterGroup:         newFilterGroupReports(),
		displayOptionsGroup: newDisplayOptionsGroupReports(interfaces.Yearly, 3, interfaces.ByAccount),
		cmdRunner:           cmdRunner,
	}
	a.newIncomeStatement()
	a.newBalanceSheet()
	a.incomeStatement.Focus()
	return a
}

func (a *reports) newIncomeStatement() {
	a.incomeStatement = newComplexTable()
	a.incomeStatement.upper.SetHeight((a.height - 20) / 2)
	a.incomeStatement.lower.SetHeight((a.height - 20) / 2)
	a.incomeStatement.bottomBar.SetHeight(1)
	a.incomeStatement.Init()
}

func (a *reports) newBalanceSheet() {
	a.balanceSheet = newComplexTable()
	// Enable Balance Sheet-specific styling: purple headers and no row highlight
	a.balanceSheet.isBalanceSheet = true
	a.balanceSheet.upper.SetHeight((a.height - 20) / 2)
	a.balanceSheet.lower.SetHeight((a.height - 20) / 2)
	a.balanceSheet.bottomBar.SetHeight(1)
	a.balanceSheet.Init()
}

func (a *reports) Init() tea.Cmd {
	return tea.Sequence(
		a.incomeStatement.Init(),
		a.balanceSheet.Init(),
		a.updateReportsCmd,
	)
}

func (a *reports) Update(msg tea.Msg) (tea.Model, tea.Cmd) {
	var cmd tea.Cmd
	switch msg := msg.(type) {
	case tea.WindowSizeMsg:
		log.Printf("reports: msg: %T", msg)
		a.height = msg.Height
		a.width = msg.Width

		fg, _ := a.filterGroup.Update(msg)
		a.filterGroup = fg.(*filterGroup)

		a.incomeStatement.upper.SetHeight((a.height - 20) / 2)
		a.incomeStatement.lower.SetHeight((a.height - 20) / 2)
		a.incomeStatement.bottomBar.SetHeight(1)

		a.balanceSheet.upper.SetHeight((a.height - 20) / 2)
		a.balanceSheet.lower.SetHeight((a.height - 20) / 2)
		a.balanceSheet.bottomBar.SetHeight(1)

		// only set column sizes, no update to row contents
		setColumns(a.incomeStatement, msg.Width)
		setColumns(a.balanceSheet, msg.Width)
		return a, nil

	case focusFilterMsg:
		log.Printf("reports: msg: %T", msg)
		a.filterGroup.Focus()
		return a, nil

	case blurFilterMsg:
		log.Printf("reports: msg: %T", msg)
		a.filterGroup.Blur()
		return a, nil

	case refreshDataMsg:
		log.Printf("reports: msg: %T", msg)
		a.filterGroup.Blur()
		return a, a.updateReportsCmd

case tea.KeyMsg:
		log.Printf("reports: msg: %T | %v", msg, msg)
		if a.filterGroup.Focused() {
			fg, cmd := a.filterGroup.Update(msg)
			a.filterGroup = fg.(*filterGroup)
			return a, cmd
		}
		switch msg.String() {
		case "1":
			a.incomeStatement.Focus()
			a.balanceSheet.Blur()
			// leaving B/S graph mode when switching away
			a.showBSGraph = false
		case "2":
			a.incomeStatement.Blur()
			a.balanceSheet.Focus()
		case "g", "G":
			// Toggle B/S graphs only when Balance Sheet is active
			if a.balanceSheet != nil && a.balanceSheet.Focused() {
				a.showBSGraph = !a.showBSGraph
				if a.showBSGraph {
					// Start rendering graph asynchronously
					a.bsGraphReady = false
					f := func() tea.Msg {
						// Use available width/height of right panel approximately
						w := a.width
						h := a.height / 2 * 2
						img, err := GenerateBSGraphs(2025, w, h)
						if err != nil { return updateBSCharts{content: fmt.Sprintf("Error: %v", err)} }
						return updateBSCharts{content: img}
					}
					a.cmdRunner.Run(f)
				}
				return a, nil
			}

		default:
			dg, cmd := a.displayOptionsGroup.Update(msg)
			a.displayOptionsGroup = dg.(*displayOptionsGroup)
			if cmd != nil {
				return a, cmd
			}

			// standard table update so default keybindings work
			a.balanceSheet, _ = a.balanceSheet.Update(msg)
			a.incomeStatement, _ = a.incomeStatement.Update(msg)
			return a, nil
		}

	case updateReports:
		log.Printf("reports: msg: %T", msg)
		return a, tea.Batch(queryIncomeStatementCmd, queryBalanceSheetCmd)

	case queryIncomeStatement:
		log.Printf("reports: msg: %T", msg)
		f := func() tea.Msg {
			data := a.setIncomeStatementData()
			return updateIncomeStatement{data: data}
		}
		a.cmdRunner.Run(f)
		return a, nil

case updateIncomeStatement:
		log.Printf("reports: msg: %T", msg)
		if msg.data != nil {
			updateComplexTable(a.incomeStatement, msg.data, a.width)
		} else {
			log.Printf("reports: income statement data is nil; skipping update to avoid crash")
		}
		return a, nil

	case queryBalanceSheet:
		log.Printf("reports: msg: %T", msg)
		f := func() tea.Msg {
			data := a.setBalanceSheetData()
			return updateBalanceSheet{data: data}
		}
		a.cmdRunner.Run(f)
		return a, nil

case updateBalanceSheet:
		log.Printf("reports: msg: %T", msg)
		if msg.data != nil {
			updateComplexTable(a.balanceSheet, msg.data, a.width)
		} else {
			log.Printf("reports: balance sheet data is nil; skipping update to avoid crash")
		}
		return a, nil

	case updateBSCharts:
		log.Printf("reports: msg: %T", msg)
		a.bsGraphReady = true
		a.bsGraphString = msg.content
		return a, nil
	}

	return a, cmd
}

type queryIncomeStatement struct{}

func queryIncomeStatementCmd() tea.Msg {
	return queryIncomeStatement{}
}

type updateIncomeStatement struct {
	data *interfaces.ComplexTable
}

type queryBalanceSheet struct{}

func queryBalanceSheetCmd() tea.Msg {
	return queryBalanceSheet{}
}

type updateBalanceSheet struct {
	data *interfaces.ComplexTable
}

type updateBSCharts struct {
	content string
}

type updateReports struct{}

func (a *reports) updateReportsCmd() tea.Msg {
	return updateReports{}
}

func (a *reports) setIncomeStatementData() *interfaces.ComplexTable {
	filter := interfaces.Filter{
		Account:     a.filterGroup.AccountName(),
		DateStart:   a.filterGroup.DateStart(),
		DateEnd:     a.filterGroup.DateEnd(),
		Description: a.filterGroup.Description(),
	}

	displayOptions := interfaces.DisplayOptions{
		Interval: a.displayOptionsGroup.IntervalValue(),
		Depth:    a.displayOptionsGroup.DepthValue(),
		Sort:     a.displayOptionsGroup.SortValue(),
	}

	data, err := a.dataProvider.IncomeStatement(filter, displayOptions)
	if err != nil {
		log.Printf("error: %v", err)
		return nil
	}

	// Customize Expenses view: add the previous two months and ensure Average is YTD monthly avg
	data = a.enrichExpensesWithPrevTwoMonthsAndAverage(data, filter, displayOptions)

	return data
}

func (a *reports) setBalanceSheetData() *interfaces.ComplexTable {
	filter := interfaces.Filter{
		Account:     a.filterGroup.AccountName(),
		DateStart:   a.filterGroup.DateStart(),
		DateEnd:     a.filterGroup.DateEnd(),
		Description: a.filterGroup.Description(),
	}

	displayOptions := interfaces.DisplayOptions{
		Interval: a.displayOptionsGroup.IntervalValue(),
		Depth:    a.displayOptionsGroup.DepthValue(),
		Sort:     a.displayOptionsGroup.SortValue(),
	}

	data, err := a.dataProvider.BalanceSheet(filter, displayOptions)
	if err != nil {
		log.Printf("error: %v", err)
		return nil
	}

	return data
}

func (a *reports) View() string {
	var view string
	var title string

	if a.incomeStatement.Focused() {
		view = a.incomeStatement.View()
		title = lipgloss.JoinHorizontal(
			lipgloss.Top,
			activeTitleStyle.Render("(1) Income Statement"),
			inactiveTitleStyle.Render("(2) Balance Sheet"),
		)
	} else {
		// Balance Sheet
		if a.showBSGraph {
			if !a.bsGraphReady {
				view = lipgloss.NewStyle().Foreground(lipgloss.Color("240")).Render("Rendering B/S charts...")
			} else {
				view = a.bsGraphString
			}
		} else {
			view = a.balanceSheet.View()
		}
		title = lipgloss.JoinHorizontal(
			lipgloss.Top,
			inactiveTitleStyle.Render("(1) Income Statement"),
			activeTitleStyle.Render("(2) Balance Sheet"),
		)
	}

	filterView := lipgloss.JoinHorizontal(
		lipgloss.Center,
		a.filterGroup.View(),
		" ",
		lipgloss.NewStyle().
			Border(lipgloss.RoundedBorder(), false, false, false, true).
			BorderForeground(lipgloss.Color("240")).
			Render(divider.View()),
		" ",
		a.displayOptionsGroup.View(),
	)
	return lipgloss.JoinVertical(
		lipgloss.Left,
		filterView,
		lipgloss.JoinVertical(
			lipgloss.Left,
			title,
			view,
		),
	)
}

// enrichIncomeStatementWithQuarterAndYTD appends Q1, Q2, and YTD columns (current year)
// to the provided income statement table by querying hledger for each period
// and merging the results by account for both Revenues (upper) and Expenses (lower).
// enrichExpensesWithPrevTwoMonthsAndAverage ensures the Expenses (lower) section includes two
// additional columns for the previous two months (relative to now), inserted just after the
// account name, and ensures an "Average" column exists with the YTD monthly average for the
// current calendar year. Revenues (upper) and Net rows are padded with blanks for the added
// month columns so overall structure remains consistent.
func (a *reports) enrichExpensesWithPrevTwoMonthsAndAverage(base *interfaces.ComplexTable, baseFilter interfaces.Filter, baseDisplay interfaces.DisplayOptions) *interfaces.ComplexTable {
	if base == nil {
		return nil
	}

	// Determine previous two whole months relative to now
	now := time.Now()
	startPrev1 := time.Date(now.Year(), now.Month(), 1, 0, 0, 0, 0, time.UTC).AddDate(0, -1, 0) // first day of last month
	endPrev1Ex := time.Date(now.Year(), now.Month(), 1, 0, 0, 0, 0, time.UTC)                    // first day of this month
	startPrev2 := startPrev1.AddDate(0, -1, 0)                                                    // first day of two months ago
	endPrev2Ex := startPrev1                                                                       // end exclusive of prev2

	// Also compute YTD for monthly average
	ytdStart := time.Date(now.Year(), time.January, 1, 0, 0, 0, 0, time.UTC)
	ytdEndEx := now.AddDate(0, 0, 1)

	periods := []struct {
		label string
		from  time.Time
		toEx  time.Time
	}{
		{startPrev2.Format("Jan"), startPrev2, endPrev2Ex},
		{startPrev1.Format("Jan"), startPrev1, endPrev1Ex},
	}

	// Use aggregate view (no period breakdown) for single-month totals
	aggDisplay := interfaces.DisplayOptions{Interval: "", Depth: baseDisplay.Depth, Sort: baseDisplay.Sort}

	// Lookup maps for prev2/prev1 totals
	type monthMaps struct { upper, lower map[string]string }
	mLookup := make([]monthMaps, len(periods))
	for i, p := range periods {
		f := baseFilter
		f.DateStart = p.from.Format("2006-01-02")
		f.DateEnd = p.toEx.Format("2006-01-02")
		ct, err := a.dataProvider.IncomeStatement(f, aggDisplay)
		if err != nil || ct == nil {
			log.Printf("warn: failed to fetch month %s income statement: %v", p.label, err)
			mLookup[i] = monthMaps{upper: map[string]string{}, lower: map[string]string{}}
			continue
		}
		u := map[string]string{}
		for _, row := range ct.Upper {
			if len(row) == 0 { continue }
			name := strings.ToLower(row[0])
			val := ""
			if len(row) > 1 { val = row[len(row)-1] }
			u[name] = val
		}
		l := map[string]string{}
		for _, row := range ct.Lower {
			if len(row) == 0 { continue }
			name := strings.ToLower(row[0])
			val := ""
			if len(row) > 1 { val = row[len(row)-1] }
			l[name] = val
		}
		mLookup[i] = monthMaps{upper: u, lower: l}
	}

	// Build YTD totals map for average
	ytdFilter := baseFilter
	ytdFilter.DateStart = ytdStart.Format("2006-01-02")
	ytdFilter.DateEnd = ytdEndEx.Format("2006-01-02")
	ytdCT, err := a.dataProvider.IncomeStatement(ytdFilter, aggDisplay)
	if err != nil { log.Printf("warn: failed to fetch YTD income statement: %v", err) }
	upperYTD := map[string]string{}
	lowerYTD := map[string]string{}
	if ytdCT != nil {
		for _, row := range ytdCT.Upper {
			if len(row) == 0 { continue }
			key := strings.ToLower(row[0])
			val := ""
			if len(row) > 1 { val = row[len(row)-1] }
			upperYTD[key] = val
		}
		for _, row := range ytdCT.Lower {
			if len(row) == 0 { continue }
			key := strings.ToLower(row[0])
			val := ""
			if len(row) > 1 { val = row[len(row)-1] }
			lowerYTD[key] = val
		}
	}

	// Insert month labels right after the first column header (typically Account)
	if len(base.Columns) > 0 && strings.EqualFold(strings.TrimSpace(base.Columns[0]), "account") {
		base.Columns = append([]string{base.Columns[0], periods[0].label, periods[1].label}, base.Columns[1:]...)
	} else {
		base.Columns = append([]string{periods[0].label, periods[1].label}, base.Columns...)
	}

	// Helper to insert n cells after the first cell in a row
	insertAfterFirst := func(row []string, vals ...string) []string {
		if len(row) == 0 { return append([]string{}, vals...) }
		out := make([]string, 0, len(row)+len(vals))
		out = append(out, row[0])
		out = append(out, vals...)
		out = append(out, row[1:]...)
		return out
	}

	// Pad Revenues (upper) rows with blanks for prev2/prev1
	for i := range base.Upper {
		base.Upper[i] = insertAfterFirst(base.Upper[i], "", "")
	}

	// Fill Expenses (lower) rows with prev2/prev1 values
	for i := range base.Lower {
		name := ""
		if len(base.Lower[i]) > 0 { name = strings.ToLower(base.Lower[i][0]) }
		v2 := mLookup[0].lower[name]
		v1 := mLookup[1].lower[name]
		base.Lower[i] = insertAfterFirst(base.Lower[i], v2, v1)
	}

	// Update bottom bar (Net) to keep column counts aligned
	if len(base.BottomBar) > 0 {
		base.BottomBar = insertAfterFirst(base.BottomBar, "", "")
	}

	// Ensure/compute Average as YTD monthly average for current year
	monthsElapsed := int(now.Month())
	avgHeaderExists := false
	for _, h := range base.Columns {
		if strings.EqualFold(strings.TrimSpace(h), "average") { avgHeaderExists = true; break }
	}
	if !avgHeaderExists {
		base.Columns = append(base.Columns, "Average")
	}

	// Helper to set/append last cell
	setOrAppendLast := func(row []string, v string) []string {
		if avgHeaderExists {
			if len(row) == 0 { return []string{"", v} }
			row[len(row)-1] = v
			return row
		}
		return append(row, v)
	}

	// Compute averages for Expenses
	for i := range base.Lower {
		name := ""
		if len(base.Lower[i]) > 0 { name = strings.ToLower(base.Lower[i][0]) }
		// Pick a currency prefix from existing row values if any
		currency := ""
		for _, c := range base.Lower[i][1:] { if strings.TrimSpace(c) != "" { currency = detectCurrencyPrefix(c); break } }
		if currency == "" { currency = detectCurrencyPrefix(lowerYTD[name]) }
		val := parseAmount(lowerYTD[name])
		avg := 0.0
		if monthsElapsed > 0 { avg = val / float64(monthsElapsed) }
		base.Lower[i] = setOrAppendLast(base.Lower[i], formatCurrency(currency, avg))
	}
	// Keep Revenues' average blank/aligned
	for i := range base.Upper {
		base.Upper[i] = setOrAppendLast(base.Upper[i], "")
	}
	// Bottom bar avg blank
	if len(base.BottomBar) > 0 { base.BottomBar = setOrAppendLast(base.BottomBar, "") }

	return base
}

// detectCurrencyPrefix tries to extract a non-numeric prefix (e.g., "$", "USD", "SGD$") from an amount string.
func detectCurrencyPrefix(s string) string {
	s = strings.TrimSpace(s)
	if s == "" { return "" }
	// Strip surrounding parentheses
	if strings.HasPrefix(s, "(") && strings.HasSuffix(s, ")") {
		s = strings.TrimSuffix(strings.TrimPrefix(s, "("), ")")
	}
	// Trim leading minus from the prefix area if present
	s = strings.TrimSpace(s)
	for i, r := range s {
		if (r >= '0' && r <= '9') || r == '.' {
			pref := strings.TrimSpace(s[:i])
			pref = strings.TrimSuffix(pref, "-")
			return pref
		}
	}
	return ""
}

func formatCurrency(prefix string, v float64) string {
	if prefix != "" {
		return fmt.Sprintf("%s%.2f", prefix, v)
	}
	return fmt.Sprintf("%.2f", v)
}
