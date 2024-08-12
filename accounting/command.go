package accounting

import (
	"bytes"
	"fmt"
	"log"
	"os/exec"
	"strings"

	"github.com/siddhantac/hledger"
)

func RunCommand(command string, options hledger.Options) (*bytes.Buffer, error) {
	args := strings.Split(command, " ")

	opts := options.Build()
	args = append(args, opts...)
	log.Printf("cmd: %v", args)

	cmd := exec.Command(args[0], args[1:]...)
	result, err := cmd.CombinedOutput()
	if err != nil {
		return nil, fmt.Errorf("%s (%s)", string(result), err)
	}

	return bytes.NewBuffer(result), nil
}
