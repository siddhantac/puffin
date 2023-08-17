//go:build !windows

package hledger

func hledgerExecutable() string { return "hledger" }
