package ui

import (
	"fmt"
	"log"
	"os/exec"
	"strings"

	"github.com/siddhantac/hledger"
)

func runCommand(cmd string) func(options hledger.Options) string {
	return func(options hledger.Options) string {
		args := strings.Split(cmd, " ")

		opts := options.Build()
		args = append(args, opts...)
		log.Printf("cmd: %v", args)

		cmd := exec.Command(args[0], args[1:]...)
		result, err := cmd.CombinedOutput()
		if err != nil {
			return fmt.Sprintf("%s (%s)", string(result), err)
		}

		return string(result)
	}
}

func detectCommand(id int, report Report) ContentModel {
	args := strings.Split(report.Cmd, " ")
	switch args[1] {
	case "balance":
		log.Printf("create tableGraph")
		tg := newTableGraph(id, report.Name, report.Locked, runCommand(report.Cmd), cmdBalance)
		return newMainView(id, report.Name, tg)
	case "register":
		log.Printf("create table")
		tbl := newTable("register", nil, id, runCommand(report.Cmd), report.Locked, cmdRegister)
		return newMainView(id, report.Name, tbl)
	case "accounts":
		log.Printf("create pager")
		pg := newPager(id, report.Name, report.Locked, runCommand(report.Cmd), cmdAccounts)
		return newMainView(id, report.Name, pg)
	default:
		log.Printf("create pager")
		pg := newPager(id, report.Name, report.Locked, runCommand(report.Cmd), cmdUnknown)
		return newMainView(id, report.Name, pg)
	}
}
