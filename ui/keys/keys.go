package keys

import "github.com/charmbracelet/bubbles/key"

var (
	Up         = key.NewBinding(key.WithKeys("up"))
	Down       = key.NewBinding(key.WithKeys("down"))
	ScrollUp   = key.NewBinding(key.WithKeys("shift+up"))
	ScrollDown = key.NewBinding(key.WithKeys("shift+down"))

	Help = key.NewBinding(key.WithKeys("?"))
	// Quit the application with q or Ctrl+C
	Quit = key.NewBinding(key.WithKeys("q", "ctrl+c"))
	Esc  = key.NewBinding(key.WithKeys("esc"))

	Refresh      = key.NewBinding(key.WithKeys("r"))
	Filter       = key.NewBinding(key.WithKeys("f"))
	ResetFilters = key.NewBinding(key.WithKeys("x"))

	AcctDepthDecr = key.NewBinding(key.WithKeys("-"))
	AcctDepthIncr = key.NewBinding(key.WithKeys("+"))
	TreeView      = key.NewBinding(key.WithKeys("t"))
	SortBy        = key.NewBinding(key.WithKeys("s"))
	ShowGraph     = key.NewBinding(key.WithKeys("g", "G"))
	Theme         = key.NewBinding(key.WithKeys("T"))

	Weekly    = key.NewBinding(key.WithKeys("w"))
	Monthly   = key.NewBinding(key.WithKeys("m"))
	Quarterly = key.NewBinding(key.WithKeys("u"))
	Yearly    = key.NewBinding(key.WithKeys("y"))
)
