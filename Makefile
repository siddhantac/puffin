build:
	@go build -o hlui

run:
	LEDGER_FILE=data/hledger.journal go run .

start: build
	@LEDGER_FILE=data/hledger.journal ./hlui

test:
	go test ./...
