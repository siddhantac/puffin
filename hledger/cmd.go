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
	//hledgerCmd = hledgerCmd + " -f " + filepath.Join(getCwd(), "hledger.journal")
	//args := buildCmd2(hledgerCmd) //, filters...)
	// logger.Logf("running command: '%s'", cmdStr)

	args := []string{
		"balance",
		"type:a",
		"--layout",
		"bare",
		"-f",
		filepath.Join(getCwd(), "hledger.journal"),
		"-O",
		"csv",
	}

	logger.Logf("running command: %s", strings.Join(args, " "))

	cmd := exec.Command(filepath.Join(getCwd(), hledgerStr), args...)
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
		filepath.Join(getCwd(), hledgerStr),
		// hledgerStr,
		hledgerCmd,
	}

	// for _, f := range filters {
	// 	args = append(args, f.Build())
	// }

	args = append(args, "-O", "csv")

	return args
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
