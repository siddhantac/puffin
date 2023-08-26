package logger

import (
	"log"
	"os"
)

func Log(s string) {
	Logf("%s\n", s)
}

func Logf(formatStr string, params ...interface{}) {
	if os.Getenv("DEBUG") != "" {
		log.Printf(formatStr, params...)
	}
}
