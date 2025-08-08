package keys

import "github.com/charmbracelet/bubbles/key"

var (
	Up         = key.NewBinding(key.WithKeys("up", "k"))
	Down       = key.NewBinding(key.WithKeys("down", "j"))
	ScrollUp   = key.NewBinding(key.WithKeys("K", "shift+up"))
	ScrollDown = key.NewBinding(key.WithKeys("J", "shift+down"))

	Help = key.NewBinding(key.WithKeys("?"))
	Quit = key.NewBinding(key.WithKeys("q", "ctrl+c"))
	Esc  = key.NewBinding(key.WithKeys("esc"))

	Refresh      = key.NewBinding(key.WithKeys("r"))
	Filter       = key.NewBinding(key.WithKeys("f"))
	ResetFilters = key.NewBinding(key.WithKeys("x"))

	AcctDepthDecr = key.NewBinding(key.WithKeys("-"))
	AcctDepthIncr = key.NewBinding(key.WithKeys("+"))
	TreeView      = key.NewBinding(key.WithKeys("t"))
	SortBy        = key.NewBinding(key.WithKeys("s"))
	ShowGraph     = key.NewBinding(key.WithKeys("g"))
	Theme         = key.NewBinding(key.WithKeys("T"))

	Weekly    = key.NewBinding(key.WithKeys("w"))
	Monthly   = key.NewBinding(key.WithKeys("m"))
	Quarterly = key.NewBinding(key.WithKeys("u"))
	Yearly    = key.NewBinding(key.WithKeys("y"))
)
