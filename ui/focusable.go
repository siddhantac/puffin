package ui

type focusable interface {
	Focus()
	Blur()
	Focused() bool
}

type periodFilterFocusable struct {
	focused bool
}

func (pf *periodFilterFocusable) Focus()        { pf.focused = true }
func (pf *periodFilterFocusable) Focused() bool { return pf.focused }
func (pf *periodFilterFocusable) Blur()         { pf.focused = false }
