package hledger_test

import (
	"puffin/hledger"
	"testing"
)

func TestLastNMonths(t *testing.T) {
	df := hledger.NewDateFilter()
	sixMonths := df.LastNMonths(6)
    val := sixMonths.Build()
    expected := "date:2023/07.."
    if val != expected {
        t.Fatalf("expected=%v, got=%v", expected, val)
    }
}
