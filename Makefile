BINARY = puffin
VERSION := $(shell git describe --tags --abbrev=0)

debug:
	rm -f puffin.log
	go run . -debug -cfg config.debug.json
	
build:
	@go build -ldflags="-X puffin/ui.Version=$(VERSION)" -o $(BINARY)

lint:
	golangci-lint run

run:
	go run . -cfg config.json

start: build
	./$(BINARY)

test:
	go test ./...

demo:
	vhs demo.tape

release:
	goreleaser release --clean

all: lint build demo
