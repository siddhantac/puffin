package accounting

import (
	"bytes"
	"os/exec"
	"strings"

	"github.com/siddhantac/hledger"
)

func RunCommand(command string, options hledger.Options) (*bytes.Buffer, error) {
	args := strings.Split(command, " ")

	opts := options.Build()
	args = append(args, opts...)

	cmd := exec.Command(args[0], args[1:]...)
	result, err := cmd.CombinedOutput()
	if err != nil {
		return bytes.NewBuffer(result), err
	}

	return bytes.NewBuffer(result), nil
}
