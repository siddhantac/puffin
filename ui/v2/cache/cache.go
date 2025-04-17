package cache

import (
	"puffin/ui/v2/interfaces"
)

type Cache struct {
	dataProvider       interfaces.DataProvider
	accountBalances    [][]string
	subAccountBalances map[string][][]string
	records            map[string][][]string
	incomeStatement    map[string]*interfaces.ComplexTable
	balanceSheet       map[string]*interfaces.ComplexTable
}

func NewCache(dataProvider interfaces.DataProvider) *Cache {
	return &Cache{
		dataProvider:       dataProvider,
		subAccountBalances: make(map[string][][]string),
		records:            make(map[string][][]string),
		incomeStatement:    make(map[string]*interfaces.ComplexTable),
		balanceSheet:       make(map[string]*interfaces.ComplexTable),
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

func (c *Cache) SubAccountBalances(filter interfaces.Filter) ([][]string, error) {
	if c.subAccountBalances[cacheKey(filter)] != nil {
		return c.subAccountBalances[cacheKey(filter)], nil
	}
	data, err := c.dataProvider.SubAccountBalances(filter)
	if err != nil {
		return nil, err
	}
	c.subAccountBalances[cacheKey(filter)] = data
	return data, nil
}

func (c *Cache) Records(filter interfaces.Filter) ([][]string, error) {
	if c.records[cacheKey(filter)] != nil {
		return c.records[cacheKey(filter)], nil
	}
	data, err := c.dataProvider.Records(filter)
	if err != nil {
		return nil, err
	}
	c.records[cacheKey(filter)] = data
	return data, nil
}

func (c *Cache) IncomeStatement(filter interfaces.Filter, displayOptions interfaces.DisplayOptions) (*interfaces.ComplexTable, error) {
	if c.incomeStatement[cacheKey(filter)] != nil {
		return c.incomeStatement[cacheKey(filter)], nil
	}
	data, err := c.dataProvider.IncomeStatement(filter, displayOptions)
	if err != nil {
		return nil, err
	}
	c.incomeStatement[cacheKey(filter)] = data
	return data, nil
}

func (c *Cache) BalanceSheet(filter interfaces.Filter, displayOptions interfaces.DisplayOptions) (*interfaces.ComplexTable, error) {
	if c.balanceSheet[cacheKey(filter)] != nil {
		return c.balanceSheet[cacheKey(filter)], nil
	}
	data, err := c.dataProvider.BalanceSheet(filter, displayOptions)
	if err != nil {
		return nil, err
	}
	c.balanceSheet[cacheKey(filter)] = data
	return data, nil
}

func cacheKey(filter interfaces.Filter) string {
	return filter.AccountType + filter.Account + filter.DateStart + filter.DateEnd + filter.Description
}
