# Puffin

Terminal dashboard to manage personal finances. Built with [hledger](https://hledger.org/) and [bubbletea](https://github.com/charmbracelet/bubbletea).

<p>
    <a href="https://github.com/siddhantac/puffin/releases"><img src="https://img.shields.io/github/release/siddhantac/puffin.svg" alt="Latest Release"></a>
    <a href="https://github.com/siddhantac/puffin/actions/workflows/go.yml"><img src="https://github.com/siddhantac/puffin/actions/workflows/go.yml/badge.svg" alt="Build Status"></a>
</p>
    
<a href="./altscreen-toggle/main.go">
  <img width="750" src="gifs/demo.gif" />
</a>

## Features
- View balance
    - assets
    - expenses
    - revenue
    - liabilities
- View reports
    - income statement
    - balance sheet
- View transactions
- Filter by 
    - account name
    - start/end date
- View by period (monthly, yearly)
- Zoom in/out of accounts (change account depth)


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

**Custom reports**

You can setup custom reports using a config file.

```
puffin -cfg config.json
```

See [config.json](config.json) for ideas.


### Keys

| Key | Feature |
| --- | --- |
| <kbd>?</kbd> | toggle help (to remove) |
| <kbd>q</kbd> | quit app |
| <kbd>r</kbd> | refresh data |
| <kbd>f</kbd> | activate filters |
| <kbd>esc</kbd> | de-activate filter |
| <kbd>j/k/up/down</kbd> | navigate reports |
| <kbd>up/down</kbd> | navigate filters (when active) |
| <kbd>J/K/PgUp/PgDown</kbd> | scroll inside a report |
| <kbd>m/u/y</kbd> | monthly/quarterly/yearly report |

