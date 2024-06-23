BINARY = puffin
VERSION := $(shell git describe --tags --abbrev=0)

debug:
	rm -f puffin.log
	LEDGER_FILE=data/hledger.journal go run . -debug
	
build:
	@go build -ldflags="-X puffin/ui.Version=$(VERSION)" -o $(BINARY)

lint:
	golangci-lint run

run:
	LEDGER_FILE=data/hledger.journal go run .

start: build
	@LEDGER_FILE=data/hledger.journal ./$(BINARY)

test:
	go test ./...

demo:
	vhs gifs/demo.tape

all: lint build demo
