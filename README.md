# Puffin
A bubbletea based TUI to manage personal finances using hledger

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

### Pre-requisite

[hledger](https://hledger.org/) is required for puffin to work.

### Run

* Clone this repo
* Run `make start`

## Features
- View transactions
- View account balance
- Filter transactions and balance by 
    - account name
    - date (smart queries only, eg. `last month` , `this year`)

### Keys

| Key | Feature |
| --- | --- |
| `/` | search by description |
    
## Planned
- Add new transactions
- Filter by exact dates (eg. `2022/10`, `2021/06/23` etc)
- View reports (needs to be fleshed out more)
- Change account depth using +/-

