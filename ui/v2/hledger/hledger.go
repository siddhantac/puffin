package hledger

import (
	"encoding/csv"
	"errors"
	"fmt"
	"io"
	"log"
	"os/exec"
	"strconv"
	"time"

	"github.com/siddhantac/puffin/ui/v2/interfaces"
)

type HledgerData struct {
}

func (hd HledgerData) runCommand(args []string) (io.Reader, error) {
	log.Printf("data: command: %v", args)
	cmd := exec.Command("hledger", args...)
	stdout, err := cmd.StdoutPipe()
	if err != nil {
		return nil, fmt.Errorf("failed to get stdout pipe: %w", err)
	}
	if err := cmd.Start(); err != nil {
		return nil, fmt.Errorf("failed to start command: %w", err)
	}

	go func() {
		if err := cmd.Wait(); err != nil {
			log.Fatal(err)
		}
	}()

	return stdout, err
}

func (hd HledgerData) parseCSV(r io.Reader, modifiers ...modifier) ([][]string, error) {
	result := make([][]string, 0)
	csvrdr := csv.NewReader(r)
	// csvrdr.Read() // skip 1 line
	for {
		rec, err := csvrdr.Read()
		if errors.Is(err, io.EOF) {
			break
		}
		if err != nil {
			return nil, fmt.Errorf("failed to read: %w", err)
		}
		for _, modify := range modifiers {
			rec = modify(rec)
		}
		result = append(result, rec)
	}
	return result, nil
}

func (hd HledgerData) Balance(filter interfaces.Filter, displayOptions interfaces.DisplayOptions) ([][]string, error) {
	// Special handling for liabilities: show multiple date columns
	if filter.AccountType == "type:l" {
		return hd.balanceWithDateColumns(filter, displayOptions)
	}
	
	// Default behavior for non-liabilities
	args := []string{"balance", filter.AccountType, "--layout=bare", "-O", "csv"}
	filters := prepareFilters(filter.Account, filter.DateStart, filter.DateEnd, "")
	args = append(args, filters...)

	options := argsFromDisplayOptions(displayOptions)
	args = append(args, options...)

	r, err := hd.runCommand(args)
	if err != nil {
		return nil, fmt.Errorf("failed to run command: %w", err)
	}

	rows, err := hd.parseCSV(r)
	if err != nil {
		return nil, fmt.Errorf("failed to parse csv: %w", err)
	}

	return rows, nil
}

func (hd HledgerData) Records(filter interfaces.Filter) ([][]string, error) {
	args := []string{"aregister", "-O", "csv"}
	filters := prepareFilters(filter.Account, filter.DateStart, filter.DateEnd, filter.Description)
	args = append(args, filters...)

	r, err := hd.runCommand(args)
	if err != nil {
		return nil, fmt.Errorf("failed to run command: %w", err)
	}

	rows, err := hd.parseCSV(r, columnSelector)
	if err != nil {
		return nil, fmt.Errorf("failed to parse csv: %w", err)
	}

	return rows, nil
}

func (hd HledgerData) IncomeStatement(filter interfaces.Filter, displayOptions interfaces.DisplayOptions) (*interfaces.ComplexTable, error) {
	args := []string{"incomestatement", "--pretty", "-O", "csv", "--layout", "bare"}
	filters := prepareFilters(filter.Account, filter.DateStart, filter.DateEnd, "")
	args = append(args, filters...)

	options := argsFromDisplayOptions(displayOptions)
	args = append(args, options...)

	r, err := hd.runCommand(args)
	if err != nil {
		return nil, fmt.Errorf("failed to run command: %w", err)
	}

	ct, err := hd.csvToComplexTable(r)
	if err != nil {
		return nil, fmt.Errorf("failed to convert csv to complexTable: %w", err)
	}

	return ct, nil
}

func (hd HledgerData) BalanceSheet(filter interfaces.Filter, displayOptions interfaces.DisplayOptions) (*interfaces.ComplexTable, error) {
	// Use 'bs' command with monthly columns showing last 4 months: May 31, June 30, July 31, Current Day
	args := []string{"bs", "--monthly", "--average", "--pretty", "-O", "csv", "--layout", "bare"}
	
	// Calculate date range: start from 3 months ago to now
	// This will show columns for May 31, June 30, July 31, August 31, Average
	filters := prepareBalanceSheetFilters(filter.Account, filter.DateStart, filter.DateEnd, "")
	args = append(args, filters...)

	// For balance sheet, we want to show monthly data, so override some display options
	balanceSheetOptions := balanceSheetDisplayOptions(displayOptions)
	args = append(args, balanceSheetOptions...)

	r, err := hd.runCommand(args)
	if err != nil {
		return nil, fmt.Errorf("failed to run command: %w", err)
	}

	ct, err := hd.csvToComplexTable(r)
	if err != nil {
		return nil, fmt.Errorf("failed to convert csv to complexTable: %w", err)
	}

	// Customize column headers for balance sheet: replace "2025-08-31" with "Current Day"
	ct = hd.customizeBalanceSheetHeaders(ct)

	return ct, nil
}

func (hd HledgerData) csvToComplexTable(r io.Reader) (*interfaces.ComplexTable, error) {
	ct := new(interfaces.ComplexTable)
	rows, err := hd.parseCSV(r)
	if err != nil {
		return nil, fmt.Errorf("failed to parse csv: %w", err)
	}

	ct.Title = rows[0][0]
	ct.Columns = rows[1]
	ct.Upper = make([][]string, 0)
	ct.Lower = make([][]string, 0)

	index := 0
	ct.UpperTitle = rows[2][0]
	for i, row := range rows[3:] {
		ct.Upper = append(ct.Upper, row)
		if row[0] == "Total:" {
			index = i + 4 // 4 to offset because we started iterating from row 3
			break
		}
	}

	ct.LowerTitle = rows[index][0]
	for _, row := range rows[index+1 : len(rows)-1] { // start from index+1 to skip row 'Expenses'
		ct.Lower = append(ct.Lower, row)
	}
	ct.BottomBar = rows[len(rows)-1]
	return ct, nil
}

func argsFromDisplayOptions(displayOptions interfaces.DisplayOptions) []string {
	result := []string{"--depth", strconv.Itoa(displayOptions.Depth)}

	switch displayOptions.Interval {
	case interfaces.Yearly:
		result = append(result, "--yearly")
	case interfaces.Monthly:
		result = append(result, "--monthly")
		// case "quarterly":
		// 	result = append(result, "--quarterly")
		// case "weekly":
		// 	result = append(result, "--weekly")
	}

	switch displayOptions.Sort {
	case interfaces.ByAmount:
		result = append(result, "--sort")
	}

	return result
}

func prepareFilters(account, from, to, description string) []string {
	args := []string{}
	if account != "" {
		args = append(args, account)
	}
	if from != "" {
		args = append(args, "-b", from)
	}
	if to != "" {
		args = append(args, "-e", to)
	}
	if description != "" {
		args = append(args, "desc:"+description)
	}
	return args
}

func columnSelector(in []string) []string {
	return []string{in[1], in[3], in[4], in[5]}
}

// prepareBalanceSheetFilters prepares filters specifically for balance sheet with 4 months: May 31, June 30, July 31, Current Day
func prepareBalanceSheetFilters(account, from, to, description string) []string {
	args := []string{}
	if account != "" {
		args = append(args, account)
	}
	
	// Calculate start date dynamically: 3 months ago from current month
	// For example, if today is August 15, 2025, show May 31, June 30, July 31, Current Day (Aug 15)
	now := time.Now()
	threeMonthsAgo := now.AddDate(0, -3, 0)
	startDate := time.Date(threeMonthsAgo.Year(), threeMonthsAgo.Month(), 1, 0, 0, 0, 0, time.UTC)
	
	args = append(args, "-b", startDate.Format("2006-01-02"))
	
	// Set end date to today to get current day balance instead of month-end
	args = append(args, "-e", now.AddDate(0, 0, 1).Format("2006-01-02")) // tomorrow to include today
	
	if description != "" {
		args = append(args, "desc:"+description)
	}
	return args
}

// balanceSheetDisplayOptions prepares display options specifically for balance sheet
func balanceSheetDisplayOptions(displayOptions interfaces.DisplayOptions) []string {
	result := []string{"--depth", strconv.Itoa(displayOptions.Depth)}
	
	// Balance sheet always uses monthly for the multi-column view
	// Don't add --monthly here as it's already in the main args
	
	switch displayOptions.Sort {
	case interfaces.ByAmount:
		result = append(result, "--sort")
	}

	return result
}

// customizeBalanceSheetHeaders modifies column headers to show "Current Day" instead of current month-end date
func (hd HledgerData) customizeBalanceSheetHeaders(ct *interfaces.ComplexTable) *interfaces.ComplexTable {
	if len(ct.Columns) >= 5 {
		// Find the current month-end column (should be the 4th column, index 3)
		// Columns are typically: ["Account", "Commodity", "2025-05-31", "2025-06-30", "2025-07-31", "2025-08-31", "Average"]
		now := time.Now()
		currentMonth := now.Month()
		currentYear := now.Year()
		
		// Look for a column that matches the current month-end date
		for i, col := range ct.Columns {
			// Check if this column looks like a date from the current month
			if len(col) >= 7 && col[:4] == fmt.Sprintf("%d", currentYear) {
				// Parse the date to check if it's from the current month
				if parsedDate, err := time.Parse("2006-01-02", col); err == nil {
					if parsedDate.Month() == currentMonth && parsedDate.Year() == currentYear {
						// Replace this column header with "Current Day"
						ct.Columns[i] = fmt.Sprintf("%s", now.Format("2006-01-02"))
						break
					}
				}
			}
		}
	}
	return ct
}

// balanceWithDateColumns handles liabilities with multiple date columns: 7/1/2025, 8/1/2025, 8/20/2025 (today)
func (hd HledgerData) balanceWithDateColumns(filter interfaces.Filter, displayOptions interfaces.DisplayOptions) ([][]string, error) {
	// Use hledger balance command with monthly intervals to get data for specific dates
	args := []string{"balance", filter.AccountType, "--layout=bare", "-O", "csv", "--monthly"}
	
	// Calculate specific dates: 7/1/2025, 8/1/2025, 8/20/2025 (today)
	now := time.Now()
	twoMonthsAgo := now.AddDate(0, -2, 0)
	oneMonthAgo := now.AddDate(0, -1, 0)
	
	// Start from beginning of two months ago to get data for all three periods
	startDate := time.Date(twoMonthsAgo.Year(), twoMonthsAgo.Month(), 1, 0, 0, 0, 0, time.UTC)
	
	// Set filters for the date range
	args = append(args, "-b", startDate.Format("2006-01-02"))
	args = append(args, "-e", now.AddDate(0, 0, 1).Format("2006-01-02")) // tomorrow to include today
	
	// Add account filter if specified
	if filter.Account != "" {
		args = append(args, filter.Account)
	}
	
	// Add depth option
	options := argsFromDisplayOptions(displayOptions)
	args = append(args, options...)

	r, err := hd.runCommand(args)
	if err != nil {
		return nil, fmt.Errorf("failed to run command: %w", err)
	}

	rows, err := hd.parseCSV(r)
	if err != nil {
		return nil, fmt.Errorf("failed to parse csv: %w", err)
	}

	// Transform the data to match our expected column format
	// The CSV will have columns like: Account, Commodity, 2025-07-31, 2025-08-31, 2025-08-20
	// We want to show: Account, dynamic dates based on current time
	if len(rows) > 0 {
		// Calculate dynamic date headers to match what's shown in UI
		date1 := fmt.Sprintf("%d/%d/%d", twoMonthsAgo.Month(), 1, twoMonthsAgo.Year())
		date2 := fmt.Sprintf("%d/%d/%d", oneMonthAgo.Month(), 1, oneMonthAgo.Year()) 
		date3 := fmt.Sprintf("%d/%d/%d", now.Month(), now.Day(), now.Year())
		
		// Update the header row to match our custom column titles
		header := []string{"account", date1, date2, date3}
		rows[0] = header
		
		// Process data rows to remove commodity column and adjust data
		for i := 1; i < len(rows); i++ {
			if len(rows[i]) >= 3 {
				// Keep account name and the three balance columns, skip commodity
				account := rows[i][0]
				// If there are monthly columns, use them; otherwise use current balance
				balance1 := ""
				balance2 := ""
				balance3 := ""
				
				if len(rows[i]) >= 5 {
					// We have monthly data columns
					balance1 = rows[i][2] // First month
					balance2 = rows[i][3] // Second month  
					balance3 = rows[i][4] // Current period
				} else if len(rows[i]) >= 3 {
					// Only current balance available
					balance3 = rows[i][2]
					balance1 = rows[i][2] // Use same value
					balance2 = rows[i][2] // Use same value
				}
				
				rows[i] = []string{account, balance1, balance2, balance3}
			}
		}
	}

	return rows, nil
}

type modifier func([]string) []string
