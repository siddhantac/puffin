BINARY = puffin
build:
	@go build -o $(BINARY)

run:
	LEDGER_FILE=data/hledger.journal go run .

start: build
	@LEDGER_FILE=data/hledger.journal ./$(BINARY)

test:
	go test ./...

vhs:
	vhs gifs/browse.tape
