# WARP.md

This file provides guidance to WARP (warp.dev) when working with code in this repository.

## Project Overview

Puffin is a terminal dashboard for managing personal finances, built with Go using the [Bubble Tea](https://github.com/charmbracelet/bubbletea) TUI framework and [hledger](https://hledger.org/) for financial data processing.

## Common Development Commands

### Building and Running
- `make build` - Build the puffin binary
- `make start` - Build and run with default settings
- `make run` - Run with config.json configuration
- `make debug` - Run in debug mode with debug config and logging to puffin.log
- `make debugv3` - Run V3 UI in debug mode

### Testing and Quality
- `make test` - Run all Go tests
- `make lint` - Run golangci-lint for code quality checks

### Demo and Release
- `make demo` - Generate demo GIF using VHS
- `make release` - Create GitHub release using goreleaser

### Version-Specific Commands
- `./puffin -v3` - Run the new V3 interface (active development)
- `./puffin -cfg config.json` - Run with specific config file
- `./puffin -debug` - Run with debug logging enabled

## Architecture Overview

### Two UI Versions
The codebase maintains two parallel UI implementations:
- **V2 (Legacy)**: Located in `ui/` - Original implementation with extensive features
- **V3 (Current)**: Located in `ui/v2/` - New simplified architecture under active development

### Core Architecture Patterns

#### Bubble Tea Model-View-Update (MVU)
Both versions follow the Elm-inspired MVU pattern via Bubble Tea:
- **Model**: Application state (filters, settings, data)
- **Update**: State transitions based on messages
- **View**: Rendering functions that return strings

#### V3 Architecture (Current Development)
```
ui/v2/ui.go          # Main UI coordinator with tabs
├── home.go          # Home screen with 3-panel layout
├── reports.go       # Financial reports (Income Statement, Balance Sheet)
├── filter.go        # Input filtering system
├── display_option.go # Display configuration (depth, sorting, intervals)
└── hledger/         # Data provider implementation
```

#### Key Components
- **DataProvider Interface**: Abstracts financial data access (Balance, Records, IncomeStatement, BalanceSheet)
- **Filter System**: Handles account, date range, and description filtering
- **Display Options**: Manages depth, sorting, and time intervals
- **Command Runner**: Async command execution with proper concurrency

#### V2 Architecture (Legacy)
More complex with additional features:
```
ui/model.go          # Root model with navigation modes
├── tabs.go          # Tab management
├── filterGroup.go   # Advanced filtering
├── settings.go      # Configuration management
├── content_model.go # Content abstraction layer
└── command.go       # hledger command detection and execution
```

### Data Flow
1. User input → Filter/Display Options → hledger commands
2. hledger CSV output → Data parsing → Table/Chart rendering
3. Async updates via Bubble Tea message passing

### Configuration System
- JSON-based configuration in `config.json` and `config.debug.json`
- Supports custom hledger commands and report definitions
- Locked reports (unaffected by filters) vs. dynamic reports

### Theming System
- Three built-in themes: Dracula, Nord, Gruvbox
- Dynamic theme switching with `Shift+T`
- Centralized styling via lipgloss

## Key Development Notes

### Working with Financial Data
- All financial calculations delegated to hledger binary
- CSV parsing for structured data, plain text for complex reports
- Account types mapped: assets (type:a), expenses (type:x), revenue (type:r), liabilities (type:l), equity (type:e)

### State Management
- Extensive use of Bubble Tea's message passing for async operations
- Command batching for multiple simultaneous data updates
- Proper focus management across UI components

### Error Handling
- Error states displayed in UI with appropriate styling
- Graceful degradation when hledger commands fail
- Debug logging available via `-debug` flag

### Testing Data
- Sample journal file at `data/hledger.journal`
- Debug configuration points to sample data for development

### V3 Development Focus
V3 represents a simplified, more maintainable approach:
- Cleaner separation of concerns
- Simplified navigation model
- Better async handling
- More focused feature set

Use `make debugv3` or `./puffin -v3` to work with the current development version.
