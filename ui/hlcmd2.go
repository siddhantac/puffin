package ui

import (
	"io"

	hlgo "github.com/siddhantac/hledger"
)

type hlfunc func(hlgo.Options) (io.Reader, error)

func processHL2(f hlfunc, opts hlgo.Options) ([]byte, error) {
	reader, err := f(opts)
	if err != nil {
		return nil, err
	}

	b, err := io.ReadAll(reader)
	if err != nil {
		return nil, err
	}

	return b, nil
}
