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

debugv3:
	go run . -debug -v3

test:
	go test ./...

demo:
	vhs demo.tape

demov3:
	vhs home_v3.tape
	vhs reports_v3.tape

release:
	GITHUB_TOKEN=$(GITHUB_TOKEN) goreleaser release --clean

all: lint build demo
