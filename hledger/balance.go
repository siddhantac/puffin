package hledger

func (h Hledger) Balance(filters ...Filter) ([][]string, error) {
	rd, err := execCmd("balance", true, filters...)
	if err != nil {
		return nil, err
	}

	data := parseCSV(rd, 0)
	return data, nil
}
