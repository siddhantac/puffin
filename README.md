# Puffin
TUI to manage your personal finances. Uses [hledger](https://hledger.org/) and [bubbletea](https://github.com/charmbracelet/bubbletea).

**Note**: The actual financial application used is hledger, so this will NOT work without hledger. This project is a TUI to make it easier to interact with hledger.

**Table Of Contents**
- [Demo](#demo)
- [Installation](#installation)
    - [Pre-requisites](#pre-requisites)
    - [Build](#build)
    - [Run demo](#run-demo)
    - [Run with your own journal file](#run-with-your-own-journal-file)
- [Features](#features)
- [Planned](#planned)

## Demo

### Browsing transactions and balance

<a href="./altscreen-toggle/main.go">
  <img width="750" src="gifs/browse.gif" />
</a>

### Account filter

<a href="./altscreen-toggle/main.go">
  <img width="750" src="gifs/account_filter.gif" />
</a>

### Date filter

<a href="./altscreen-toggle/main.go">
  <img width="750" src="gifs/date_filter.gif" />
</a>

## Installation

### Pre-requisites

- [hledger](https://hledger.org/) is required for puffin to work.
- make
- Go compiler (>=1.17)

### Build

* Clone this repo
* Run the command `make build`. This creates the binary `puffin`.

### Run demo

* Clone this repo
* Run `make start`

### Run with your own journal file

* Build using `make build`
* Run using `./puffin`. It automatically uses the `$LEDGER_FILE` environment variable

**Custom path for the journal file**

There are 2 ways to use a custom path for the journal file.

1. Run with env var: `LEDGER_FILE=<custom_path> ./puffin`
2. Run with args: `./puffin -file <custom_path>`

**Change hledger executable path**

Run with `./puffin -exe <path_to_hledger>`

## Features
- View transactions
- View account balance
- Filter transactions and balance by 
    - account name
    - date

### Keys

| Key | Feature |
| --- | --- |
| <kbd>?</kbd> | toggle help (to remove) |
| <kbd>q</kbd> | quit |
| <kbd>r</kbd> | refresh data |
| <kbd>f</kbd> | activate filters |

<!-- TODO: keys to navigate up/down -->
    
## Planned
- [x] Filter by exact dates (eg. `2022/10`, `2021/06/23` etc)
- [x] Change account depth using +/-
- [x] Make filters visible
- [x] UI: Make income-statement look nicer (looks really boring)
- [ ] Make tables/pagers scrollable
- [ ] Create a new UI element to show hledger errors (eg. when running in Strict mode or balance assertion fails) - partially done
- [ ] Add new transactions
