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

Install (MELPA)
Once the package is available on MELPA, you can install via package.el:

- Using M-x:
  1) M-x package-refresh-contents
  2) M-x package-install RET puffin RET
  3) M-x puffin-start

- Using use-package in init.el:

  (use-package puffin
    :ensure t
    :commands (puffin-start))

Install (straight.el)
If you use straight.el, you can install directly from this repo (before or after MELPA acceptance):

  (straight-use-package
   '(puffin :type git :host github :repo "Abu-Daud/puffin" :files ("emacs/puffin/*.el")))

Install (local)
1) Add this directory to your load-path, for example in init.el:
   (add-to-list 'load-path "/Users/abudaud/mypuffin/puffin/emacs/puffin/")
2) Require and run:
   (require 'puffin)
   M-x puffin-start

Quick start
- Launch: M-x puffin-start
- Optional: point to a config file in your init.el before launch:

  (setq puffin-config-file "/path/to/config.json")

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

