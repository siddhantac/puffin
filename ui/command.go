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

func detectCommand(id int, report Report) ContentModel {
	args := strings.Split(report.Cmd, " ")
	switch args[1] {
	case "balance", "bal":
		log.Printf("create tableGraph")
		tg := newTableGraph(id, report.Name, report.Locked, runCommand(report.Cmd), cmdBalance)
		return newMainView(id, report.Name, tg)
	case "register", "reg":
		log.Printf("create table")
		tbl := newTable("register", nil, id, runCommand(report.Cmd), report.Locked, cmdRegister)
		return newMainView(id, report.Name, tbl)
	case "accounts", "acc":
		log.Printf("create pager")
		pg := newPager(id, report.Name, report.Locked, runCommand(report.Cmd), cmdAccounts)
		return newMainView(id, report.Name, pg)
	default:
		log.Printf("create pager")
		pg := newPager(id, report.Name, report.Locked, runCommand(report.Cmd), cmdUnknown)
		return newMainView(id, report.Name, pg)
	}
}
