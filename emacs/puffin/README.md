# Puffin for Emacs (experimental)

This is an Emacs package that brings Puffin (V2) functionality into Emacs.
It shells out to your local hledger binary, parses results, and renders an
interactive dashboard with tabs, filters, settings, and tables.

Status
- Initial scaffold with working buffer layout, async hledger runner,
  CSV-to-table rendering, basic filters and settings, and theme switching.
- Graphs: basic placeholder (stacked bar to be iterated).
- Months selector and register inline filter: planned.

Requirements
- Emacs 27.1+
- hledger in PATH (and your journal set up, e.g. via HLEDGER_FILE)

Install (local)
1) Add this directory to your load-path, for example in init.el:
   (add-to-list 'load-path "/Users/abudaud/mypuffin/puffin/emacs/puffin/")
2) Require and run:
   (require 'puffin)
   M-x puffin-start

Configuration
- By default, the package looks for config.json in the Puffin repo root.
  Set puffin-config-file to override:
    (setq puffin-config-file "/path/to/config.json")

Key bindings (mirrors Puffin V2 where sensible)
- Up/Down: navigate reports
- r: refresh
- f: edit filters (minibuffer prompts)
- x: reset filters
- t: toggle tree-view
- + / -: change account depth
- s: toggle sort mode
- T: next theme (Dracula/Nord/Gruvbox)
- w/m/u/y: weekly/monthly/quarterly/yearly period
- g: toggle graph (placeholder)
- q: quit Puffin buffer

Notes
- This is an initial port; we’ll iterate on feature parity (register inline filter,
  months selector, richer graphs, locked report behavior UI indicators, etc.).

