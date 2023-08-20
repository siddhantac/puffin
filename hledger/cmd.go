package hledger

import (
	"bytes"
	"io"
	"os/exec"
	"puffin/logger"
	"strings"
)

func execCmd(hledgerArgs []string, filters ...Filter) (io.Reader, error) {
	args := buildCmd(hledgerArgs, filters...)
	logger.Logf("running command: %s", strings.Join(args, " "))

	cmd := exec.Command(hledgerExecutable(), args...)
	result, err := cmd.CombinedOutput()
	if err != nil {
		logger.Logf("error: %v", err)
		return bytes.NewBuffer(result), err
		// return nil, err
	}

	return bytes.NewBuffer(result), nil
}

func buildCmd(hledgerArgs []string, filters ...Filter) []string {
	args := make([]string, 0)

	args = append(args, hledgerArgs...)

	for _, f := range filters {
		args = append(args, f.Build())
	}

	args = append(args, "-O", "csv")

	return args
}
