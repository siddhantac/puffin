package ui

import (
	"fmt"
	"log"
	"os/exec"
	"strings"

	"github.com/siddhantac/hledger"
)

func runCommand(cmd string) func(id int, options hledger.Options) content {
	return func(id int, options hledger.Options) content {
		c := content{id: id}

		args := strings.Split(cmd, " ")

		opts := options.Build()
		args = append(args, opts...)
		log.Printf("cmd: %v", args)

		cmd := exec.Command(args[0], args[1:]...)
		result, err := cmd.CombinedOutput()
		if err != nil {
			log.Printf("error: %v: %s", err, string(result))
			c.err = fmt.Errorf("%s\n%v", string(result), err)
			return c
		}

		c.msg = string(result)
		return c
	}
}

func detectCommand(id int, report Report, dataTransformers []dataTransformer) ContentModel {
	if report.Locked {
		pg := newPager(id, report.Name, report.Locked, runCommand(report.Cmd), cmdUnknown)
		return newMainView(id, report.Name, pg)
	}

	args := strings.Split(report.Cmd, " ")
	switch args[1] {
	case "balance", "bal":
		tg := newTableGraph(id, report.Name, report.Locked, runCommand(report.Cmd), cmdBalance, dataTransformers)
		return newMainView(id, report.Name, tg)
case "bs":
		// Balance Sheet: treat like a table/graph with CSV parsing
		// Add a transformer to color the "Assets" and "Liabilities" headings in purple
		bsTransformers := append(dataTransformers, highlightBSHeaders{})
		tg := newTableGraph(id, report.Name, report.Locked, runCommand(report.Cmd), cmdBalanceSheet, bsTransformers)
		return newMainView(id, report.Name, tg)
	case "register", "reg":
		tbl := newTable("register", nil, id, runCommand(report.Cmd), report.Locked, cmdRegister, nil)
		return newMainView(id, report.Name, tbl)
	case "accounts", "acc":
		pg := newPager(id, report.Name, report.Locked, runCommand(report.Cmd), cmdAccounts)
		return newMainView(id, report.Name, pg)
	case "incomestatement":
		is := newIncomeStatementView(id, report.Name, report.Locked, runCommand(report.Cmd))
		return newMainView(id, report.Name, is)
	default:
		pg := newPager(id, report.Name, report.Locked, runCommand(report.Cmd), cmdUnknown)
		return newMainView(id, report.Name, pg)
	}
}
