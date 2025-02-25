package ui

import (
	"puffin/ui/v2/interfaces"
)

type accountData struct {
	name        string
	balance     string
	subBalances [][]string
	records     [][]string
}

type dataModel struct {
	assets      accountData
	equity      accountData
	expenses    accountData
	income      accountData
	liabilities accountData

	dataProvider interfaces.DataProvider
}

func NewDataModel(dataProvider interfaces.DataProvider) *dataModel {
	return &dataModel{
		dataProvider: dataProvider,
		assets:       accountData{name: "assets"},
		equity:       accountData{name: "equity"},
		expenses:     accountData{name: "expenses"},
		income:       accountData{name: "income"},
		liabilities:  accountData{name: "liabilities"},
	}
}

func (dm *dataModel) AccountBalances() ([][]string, error) {
	var result [][]string
	result = append(result, []string{dm.assets.name, dm.assets.balance})
	result = append(result, []string{dm.equity.name, dm.equity.balance})
	result = append(result, []string{dm.expenses.name, dm.expenses.balance})
	result = append(result, []string{dm.income.name, dm.income.balance})
	result = append(result, []string{dm.liabilities.name, dm.liabilities.balance})
	return result, nil
}

func (dm *dataModel) Populate() {
	accountBalances, err := dm.dataProvider.AccountBalances()
	if err != nil {
		panic(err)
	}

	for _, row := range accountBalances {
		switch row[0] {
		case "assets":
			dm.assets.balance = row[1]
			dm.assets.subBalances, err = dm.dataProvider.SubAccountBalances(row[0])
			if err != nil {
				panic(err)
			}
			dm.assets.records, err = dm.dataProvider.Records(row[0])
			if err != nil {
				panic(err)
			}
		case "equity":
			dm.equity.balance = row[1]
			dm.equity.subBalances, err = dm.dataProvider.SubAccountBalances(row[0])
			if err != nil {
				panic(err)
			}
			dm.equity.records, err = dm.dataProvider.Records(row[0])
			if err != nil {
				panic(err)
			}
		case "expenses":
			dm.expenses.balance = row[1]
			dm.expenses.subBalances, err = dm.dataProvider.SubAccountBalances(row[0])
			if err != nil {
				panic(err)
			}
			dm.expenses.records, err = dm.dataProvider.Records(row[0])
			if err != nil {
				panic(err)
			}
		case "income":
			dm.income.balance = row[1]
			dm.income.subBalances, err = dm.dataProvider.SubAccountBalances(row[0])
			if err != nil {
				panic(err)
			}
			dm.income.records, err = dm.dataProvider.Records(row[0])
			if err != nil {
				panic(err)
			}
		case "liabilities":
			dm.liabilities.balance = row[1]
			dm.liabilities.subBalances, err = dm.dataProvider.SubAccountBalances(row[0])
			if err != nil {
				panic(err)
			}
			dm.liabilities.records, err = dm.dataProvider.Records(row[0])
			if err != nil {
				panic(err)
			}
		}
	}

}
