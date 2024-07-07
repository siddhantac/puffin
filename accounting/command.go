package accounting

import (
	"bytes"
	"os/exec"
	"strings"
)

func RunCommand(command string) (*bytes.Buffer, error) {
	args := strings.Split(command, " ")

	cmd := exec.Command(args[0], args[1:]...)
	result, err := cmd.CombinedOutput()
	if err != nil {
		return bytes.NewBuffer(result), err
	}

	return bytes.NewBuffer(result), nil
}
