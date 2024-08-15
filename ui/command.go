package ui

import (
	"bytes"
	"fmt"
	"io"
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

		if err != nil {
			return err.Error()
		}
		b, err := io.ReadAll(bytes.NewBuffer(result))
		if err != nil {
			return err.Error()
		}
		return string(b)
	}
}

func detectCommand(id int, report Report) ContentModel {
	args := strings.Split(report.Cmd, " ")
	switch args[1] {
	case "balance":
		log.Printf("create tableGraph")
		return newTableGraph(id, report.Name, report.Locked, runCommand(report.Cmd), cmdBalance)
	case "register":
		log.Printf("create table")
		return newTable("register", nil, id, runCommand(report.Cmd), report.Locked, cmdRegister)
	case "accounts":
		log.Printf("create pager")
		return newPager(id, report.Name, report.Locked, runCommand(report.Cmd), cmdAccounts)
	default:
		log.Printf("create pager")
		return newPager(id, report.Name, report.Locked, runCommand(report.Cmd), cmdUnknown)
	}
}
