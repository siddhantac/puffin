package ledger

// Note: taken from https://github.com/howeyc/ledger

import (
	"bufio"
	"errors"
	"fmt"
	"io"
	"os"
	"path/filepath"
	"regexp"
	"strings"
	"time"
	"unicode"

	"github.com/alfredxing/calc/compute"
	"github.com/howeyc/ledger/decimal"
	date "github.com/joyt/godate"
)

var ErrParseTransaction = fmt.Errorf("unable to parse transaction")

// ParseLedgerFileDEPRECATED parses a ledger file and returns a list of Transactions.
func ParseLedgerFileDEPRECATED(filename string) (generalLedger []*Transaction, err error) {
	ifile, ierr := os.Open(filename)
	if ierr != nil {
		return nil, ierr
	}
	defer ifile.Close()
	parseLedgerDEPRECATED(filename, ifile, func(t *Transaction, e error) (stop bool) {
		if e != nil {
			err = e
			stop = true
			return
		}

		generalLedger = append(generalLedger, t)
		return
	})

	return
}

func ParseLedger(ledgerReader io.Reader) (Transactions, error) {
	return parseLedger(ledgerReader, "")
}

func parseLedger(ledgerReader io.Reader, filename string) (Transactions, error) {
	var lp parser
	lp.scanner = bufio.NewScanner(ledgerReader)
	lp.filename = filename

	transactions := make([]*Transaction, 0)

	var line string
	for lp.scanner.Scan() {
		line = lp.scanner.Text()

		// remove heading and tailing space from the line
		trimmedLine := strings.TrimSpace(line)
		lp.lineCount++

		var currentComment string
		// handle comments
		if commentIdx := strings.Index(trimmedLine, ";"); commentIdx >= 0 {
			currentComment = trimmedLine[commentIdx:]
			trimmedLine = trimmedLine[:commentIdx]
			trimmedLine = strings.TrimSpace(trimmedLine)
		}

		// Skip empty lines
		if len(trimmedLine) == 0 {
			if len(currentComment) > 0 {
				lp.comments = append(lp.comments, currentComment)
			}
			continue
		}

		before, after, split := strings.Cut(trimmedLine, " ")
		if !split {
			return nil, fmt.Errorf("%s:%d: %w Unable to parse payee line: %s", lp.filename, lp.lineCount, ErrParseTransaction, line)
			// if len(currentComment) > 0 {
			// 	lp.comments = append(lp.comments, currentComment)
			// }
			// continue
		}
		switch before {
		case "account":
			lp.parseAccount(after)
		case "include":
			paths, _ := filepath.Glob(filepath.Join(filepath.Dir(lp.filename), after))
			if len(paths) < 1 {
				return nil, fmt.Errorf("%s:%d: Unable to include file(%s): %w", lp.filename, lp.lineCount, after, errors.New("not found"))
			}
			for _, p := range paths {
				file, _ := os.Open(p)
				defer file.Close()
				if txns, err := parseLedger(file, p); err != nil {
					return nil, err
				} else {
					transactions = append(transactions, txns...)
				}
			}
		default:
			trans, err := lp.parseTransaction(before, after, currentComment)
			if err != nil {
				return nil, fmt.Errorf("%s:%d: %s: %w", lp.filename, lp.lineCount, ErrParseTransaction.Error(), err)
			}
			transactions = append(transactions, trans)
		}
	}
	return transactions, nil
}

// ParseLedgerOLD parses a ledger file and returns a list of Transactions.
func ParseLedgerOLD(ledgerReader io.Reader) (generalLedger []*Transaction, err error) {
	parseLedgerDEPRECATED("", ledgerReader, func(t *Transaction, e error) (stop bool) {
		if e != nil {
			err = e
			stop = true
			return
		}

		generalLedger = append(generalLedger, t)
		return
	})

	return
}

// ParseLedgerAsync parses a ledger file and returns a Transaction and error channels .
func ParseLedgerAsync(ledgerReader io.Reader) (c chan *Transaction, e chan error) {
	c = make(chan *Transaction)
	e = make(chan error)

	go func() {
		parseLedgerDEPRECATED("", ledgerReader, func(t *Transaction, err error) (stop bool) {
			if err != nil {
				e <- err
			} else {
				c <- t
			}
			return
		})

		e <- nil
		close(c)
		close(e)
	}()
	return c, e
}

// Calculation expressions are enclosed in parantheses
var calcExpr = regexp.MustCompile(`(?s) \((.*)\)`)

type parser struct {
	scanner    *bufio.Scanner
	filename   string
	lineCount  int
	comments   []string
	dateLayout string
}

func (lp *parser) parseAccount(accName string) (accountName string, err error) {
	accountName = accName

	var line string
	for lp.scanner.Scan() {
		// Read until blank line (ignore all sub-directives)
		line = lp.scanner.Text()
		// remove heading and tailing space from the line
		trimmedLine := strings.TrimSpace(line)
		lp.lineCount++

		// skip comments
		if commentIdx := strings.Index(trimmedLine, ";"); commentIdx >= 0 {
			trimmedLine = trimmedLine[:commentIdx]
		}

		// stop slurping up sub-directives on empty line
		if len(trimmedLine) == 0 {
			return
		}
	}

	return
}

func (lp *parser) parseDate(dateString string) (transDate time.Time, err error) {
	// try current date layout
	transDate, err = time.Parse(lp.dateLayout, dateString)
	if err != nil {
		// try to find new date layout
		transDate, lp.dateLayout, err = date.ParseAndGetLayout(dateString)
		if err != nil {
			err = fmt.Errorf("Unable to parse date(%s): %w", dateString, err)
		}
	}
	return
}

func (lp *parser) parseTransaction(dateString, payeeString, payeeComment string) (trans *Transaction, err error) {
	transDate, derr := lp.parseDate(dateString)
	if derr != nil {
		return nil, derr
	}
	trans = &Transaction{Payee: payeeString, Date: transDate, PayeeComment: payeeComment}

	var line string
	for lp.scanner.Scan() {
		line = lp.scanner.Text()
		// remove heading and tailing space from the line
		trimmedLine := strings.TrimSpace(line)
		lp.lineCount++

		var currentComment string
		// handle comments
		if commentIdx := strings.Index(trimmedLine, ";"); commentIdx >= 0 {
			currentComment = trimmedLine[commentIdx:]
			trimmedLine = trimmedLine[:commentIdx]
			trimmedLine = strings.TrimSpace(trimmedLine)
			if len(trimmedLine) == 0 {
				lp.comments = append(lp.comments, currentComment)
				continue
			}
		}

		if len(trimmedLine) == 0 {
			break
		}

		// Check for expr
		trimmedLine = calcExpr.ReplaceAllStringFunc(trimmedLine, func(s string) string {
			f, _ := compute.Evaluate(s)
			return fmt.Sprintf("%f", f)
		})

		var accChange Account
		accChange.Name = trimmedLine
		accChange.Comment = currentComment
		if i := strings.LastIndexFunc(trimmedLine, unicode.IsSpace); i >= 0 {
			acc := strings.TrimSpace(trimmedLine[:i])
			rg := regexp.MustCompile("[-0-9,.]+")
			amt := rg.Find([]byte(trimmedLine))
			amt2 := strings.ReplaceAll(string(amt), ",", "") // remove commas
			if decbal, derr := decimal.NewFromString(string(amt2)); derr == nil {
				accChange.Name = acc
				accChange.Balance = decbal
			}
		}

		accChange.SubAccounts = make([]string, 0)
		subaccounts := strings.Split(accChange.Name, ":")
		prev := subaccounts[0]
		accChange.SubAccounts = append(accChange.SubAccounts, prev)
		for _, sa := range subaccounts[1:] {
			concatenated := prev + ":" + sa
			accChange.SubAccounts = append(accChange.SubAccounts, concatenated)
			prev = concatenated
		}

		trans.AccountChanges = append(trans.AccountChanges, accChange)
	}

	transErr := balanceTransaction(trans)
	if transErr != nil {
		err = fmt.Errorf("Unable to balance transaction: %w", transErr)
		return
	}
	trans.Comments = lp.comments
	lp.comments = nil
	return
}

// Takes a transaction and balances it. This is mainly to fill in the empty part
// with the remaining balance.
func balanceTransaction(input *Transaction) error {
	balance := decimal.Zero
	var emptyFound bool
	var emptyAccIndex int
	if len(input.AccountChanges) < 2 {
		return fmt.Errorf("need at least two postings")
	}
	for accIndex, accChange := range input.AccountChanges {
		if accChange.Balance.IsZero() {
			if emptyFound {
				return fmt.Errorf("more than one account empty")
			}
			emptyAccIndex = accIndex
			emptyFound = true
		} else {
			balance = balance.Add(accChange.Balance)
		}
	}
	if balance.Sign() != 0 {
		if !emptyFound {
			return fmt.Errorf("no empty account to place extra balance")
		}
	}
	if emptyFound {
		input.AccountChanges[emptyAccIndex].Balance = balance.Neg()
	}

	return nil
}

func parseLedgerDEPRECATED(filename string, ledgerReader io.Reader, callback func(t *Transaction, err error) (stop bool)) (stop bool) {
	var lp parser
	lp.scanner = bufio.NewScanner(ledgerReader)
	lp.filename = filename

	var line string
	for lp.scanner.Scan() {
		line = lp.scanner.Text()

		// remove heading and tailing space from the line
		trimmedLine := strings.TrimSpace(line)
		lp.lineCount++

		var currentComment string
		// handle comments
		if commentIdx := strings.Index(trimmedLine, ";"); commentIdx >= 0 {
			currentComment = trimmedLine[commentIdx:]
			trimmedLine = trimmedLine[:commentIdx]
			trimmedLine = strings.TrimSpace(trimmedLine)
		}

		// Skip empty lines
		if len(trimmedLine) == 0 {
			if len(currentComment) > 0 {
				lp.comments = append(lp.comments, currentComment)
			}
			continue
		}

		before, after, split := strings.Cut(trimmedLine, " ")
		if !split {
			if callback(nil, fmt.Errorf("%s:%d: Unable to parse transaction: %w", lp.filename, lp.lineCount,
				fmt.Errorf("Unable to parse payee line: %s", line))) {
				return true
			}
			if len(currentComment) > 0 {
				lp.comments = append(lp.comments, currentComment)
			}
			continue
		}
		switch before {
		case "account":
			lp.parseAccount(after)
		case "include":
			paths, _ := filepath.Glob(filepath.Join(filepath.Dir(lp.filename), after))
			if len(paths) < 1 {
				callback(nil, fmt.Errorf("%s:%d: Unable to include file(%s): %w", lp.filename, lp.lineCount, after, errors.New("not found")))
				return true
			}
			for _, incpath := range paths {
				ifile, _ := os.Open(incpath)
				defer ifile.Close()
				if parseLedgerDEPRECATED(incpath, ifile, callback) {
					return true
				}
			}
		default:
			trans, transErr := lp.parseTransaction(before, after, currentComment)
			if transErr != nil {
				if callback(nil, fmt.Errorf("%s:%d: Unable to parse transaction: %w", lp.filename, lp.lineCount, transErr)) {
					return true
				}
				continue
			}
			callback(trans, nil)
		}
	}
	return false
}
