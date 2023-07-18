package hledger

func (h Hledger) IncomeStatement(filters ...Filter) ([][]string, error) {
	rd, err := execCmd("incomestatement", true, filters...)
	if err != nil {
		return nil, err
	}

	data := parseCSV(rd, 1)
	return data, nil
}
