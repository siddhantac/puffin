BINARY = puffin

debug:
	rm -f puffin.log
	LEDGER_FILE=data/hledger.journal go run . -debug
	
build:
	@go build -o $(BINARY)

lint:
	golangci-lint run

run:
	LEDGER_FILE=data/hledger.journal go run .

start: build
	@LEDGER_FILE=data/hledger.journal ./$(BINARY)

test:
	go test ./...

vhs-browse:
	vhs gifs/browse.tape

vhs-date-filter:
	vhs gifs/date_filter.tape

vhs-account-filter:
	vhs gifs/account_filter.tape

vhs: vhs-browse vhs-date-filter vhs-account-filter

all: lint build
