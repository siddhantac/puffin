package hledger

import (
	"bytes"
	"io"
	"os"
	"os/exec"
	"path/filepath"
	"puffin/logger"
	"strings"
)

type Cmd struct {
}

const (
	hledgerStr   = "hledger.exe"
	outputCSVStr = "-O csv"
)

func getCwd() string {
	ex, err := os.Executable()
	if err != nil {
		panic(err)
	}
	return filepath.Dir(ex)
}

func execCmd(hledgerCmd string, outputCSV bool, filters ...Filter) (io.Reader, error) {
	hledgerCmd = hledgerCmd + " -f " + filepath.Join(getCwd(), "hledger.journal")
	cmdStr := buildCmd(hledgerCmd) //, filters...)
	logger.Logf("running command: '%s'", cmdStr)

	cmd := exec.Command( /*"bash", "-c",*/ cmdStr)
	result, err := cmd.CombinedOutput()
	if err != nil {
		return bytes.NewBuffer(result), err
		// return nil, err
	}

	return bytes.NewBuffer(result), nil
}

func buildCmd(hledgerCmd string, filters ...Filter) string {
	args := []string{
		filepath.Join(getCwd(), hledgerStr),
		hledgerCmd,
	}

	for _, f := range filters {
		args = append(args, f.Build())
	}

	args = append(args, outputCSVStr)

	return strings.Join(args, " ")
}
