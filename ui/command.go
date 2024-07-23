package ui

import (
	"io"
	"puffin/accounting"

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
