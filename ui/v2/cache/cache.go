package cache

import (
	"puffin/ui/v2/interfaces"
)

type Cache struct {
	dataProvider       interfaces.DataProvider
	accountBalances    [][]string
	subAccountBalances map[string][][]string
	records            map[string][][]string
	incomeStatement    map[string][]byte
}

func NewCache(dataProvider interfaces.DataProvider) *Cache {
	return &Cache{
		dataProvider:       dataProvider,
		subAccountBalances: make(map[string][][]string),
		records:            make(map[string][][]string),
		incomeStatement:    make(map[string][]byte),
	}
}

func (c *Cache) AccountBalances() ([][]string, error) {
	if c.accountBalances != nil {
		return c.accountBalances, nil
	}
	data, err := c.dataProvider.AccountBalances()
	if err != nil {
		return nil, err
	}
	c.accountBalances = data
	return data, nil
}

func (c *Cache) SubAccountBalances(accountType string, filter interfaces.Filter) ([][]string, error) {
	if c.subAccountBalances[cacheKey(accountType, filter)] != nil {
		return c.subAccountBalances[cacheKey(accountType, filter)], nil
	}
	data, err := c.dataProvider.SubAccountBalances(accountType, filter)
	if err != nil {
		return nil, err
	}
	c.subAccountBalances[cacheKey(accountType, filter)] = data
	return data, nil
}

func (c *Cache) Records(account string, filter interfaces.Filter) ([][]string, error) {
	accountType := ""
	if c.records[cacheKey(accountType, filter)] != nil {
		return c.records[cacheKey(accountType, filter)], nil
	}
	data, err := c.dataProvider.Records(account, filter)
	if err != nil {
		return nil, err
	}
	c.records[cacheKey(accountType, filter)] = data
	return data, nil
}

func (c *Cache) IncomeStatement(filter interfaces.Filter) ([]byte, error) {
	if c.incomeStatement[cacheKey("", filter)] != nil {
		return c.incomeStatement[cacheKey("", filter)], nil
	}
	data, err := c.dataProvider.IncomeStatement(filter)
	if err != nil {
		return nil, err
	}
	c.incomeStatement[cacheKey("", filter)] = data
	return data, nil
}

func cacheKey(accountType string, filter interfaces.Filter) string {
	return accountType + filter.AccountName() + filter.DateStart() + filter.DateEnd() + filter.Description()
}
