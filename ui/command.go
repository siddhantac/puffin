package ui

import (
	"io"
	"puffin/accounting"
	"strings"

	"github.com/siddhantac/hledger"
)

func runCommand(cmd string) func(options hledger.Options) string {
	return func(options hledger.Options) string {
		buf, err := accounting.RunCommand(cmd, options)
		if err != nil {
			return err.Error()
		}
		b, err := io.ReadAll(buf)
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
		return newTableGraph(id, report.Name, report.Locked, runCommand(report.Cmd))
	case "register":
		return newTable("register", []int{5, 10, 30, 20, 15})
	default:
		return newGenericPager(id, report.Name, report.Locked, runCommand(report.Cmd))
	}
}
