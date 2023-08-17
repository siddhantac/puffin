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
	outputCSVStr = "-O csv"
)

func execCmd(hledgerCmd string, outputCSV bool, filters ...Filter) (io.Reader, error) {
	//hledgerCmd = hledgerCmd + " -f " + filepath.Join(getCwd(), "hledger.journal")
	//args := buildCmd2(hledgerCmd) //, filters...)
	// logger.Logf("running command: '%s'", cmdStr)

	args := []string{
		"balance",
		"type:a",
		"--layout",
		"bare",
		"-f",
		"hledger.journal",
		"-O",
		"csv",
	}

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

func buildCmd2(hledgerCmd string, filters ...Filter) []string {
	args := []string{
		hledgerExecutable(),
		// hledgerStr,
		hledgerCmd,
	}

	// for _, f := range filters {
	// 	args = append(args, f.Build())
	// }

	return args
}

func buildCmd(hledgerCmd string, filters ...Filter) string {
	args := []string{
		hledgerExecutable(),
		hledgerCmd,
	}

	for _, f := range filters {
		args = append(args, f.Build())
	}

	args = append(args, "-O", "csv")

	return strings.Join(args, " ")
}
