package hledger

type Hledger struct {
	journalFilename string
}

func New() Hledger {
	return Hledger{}
}

func NewWithJournalFile(filename string) Hledger {
	return Hledger{journalFilename: filename}
}
