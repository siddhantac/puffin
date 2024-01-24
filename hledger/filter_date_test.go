package accounting_test

import (
	"puffin/accounting"
	"testing"
)

func TestLastNMonths(t *testing.T) {
	df := accounting.NewDateFilter()
	sixMonths := df.LastNMonths(6)
    val := sixMonths.Build()
    expected := "date:2023/07.."
    if val != expected {
        t.Fatalf("expected=%v, got=%v", expected, val)
    }
}
