package hledger

import (
	"bytes"
	"io"
	"os/exec"
	"puffin/logger"
	"strings"
)

type Cmd struct {
}

const (
	hledgerStr   = "hledger"
	outputCSVStr = "-O csv"
)

func execCmd(hledgerCmd string, outputCSV bool, filters ...Filter) (io.Reader, error) {
	cmdStr := buildCmd(hledgerCmd, filters...)
	logger.Logf("running command: '%s'", cmdStr)

	cmd := exec.Command("bash", "-c", cmdStr)
	result, err := cmd.CombinedOutput()
	if err != nil {
		return bytes.NewBuffer(result), err
		// return nil, err
	}

	return bytes.NewBuffer(result), nil
}

func buildCmd(hledgerCmd string, filters ...Filter) string {
	args := []string{
		hledgerStr,
		hledgerCmd,
	}

	for _, f := range filters {
		args = append(args, f.Build())
	}

	args = append(args, outputCSVStr)

	return strings.Join(args, " ")
}
