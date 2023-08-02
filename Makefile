BINARY = puffin
build:
	@go build -o $(BINARY)

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
