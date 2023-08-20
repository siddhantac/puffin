package hledger

import (
	"bytes"
	"io"
	"os/exec"
	"puffin/logger"
	"strings"
)

func (h Hledger) execCmd(hledgerArgs []string, filters ...Filter) (io.Reader, error) {
	args := h.buildCmd(hledgerArgs, filters...)
	logger.Logf("running command: %s", strings.Join(args, " "))

	cmd := exec.Command(h.HledgerBinary, args...)
	result, err := cmd.CombinedOutput()
	if err != nil {
		logger.Logf("error: %v", err)
		return bytes.NewBuffer(result), err
		// return nil, err
	}

	return bytes.NewBuffer(result), nil
}

func (h Hledger) buildCmd(hledgerArgs []string, filters ...Filter) []string {
	args := make([]string, 0)

	args = append(args, hledgerArgs...)

	for _, f := range filters {
		if _, ok := f.(NoFilter); ok {
			continue
		}
		args = append(args, f.Build())
	}

	if h.JournalFilename != "" {
		args = append(args, "-f", h.JournalFilename)
	}

	args = append(args, "-O", "csv")

	return args
}
