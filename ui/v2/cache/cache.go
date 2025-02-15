package cache

import "puffin/ui/v2/interfaces"

type Cache struct {
	dataProvider       interfaces.DataProvider
	accountBalances    [][]string
	subAccountBalances map[string][][]string
	records            map[string][][]string
}

func NewCache(dataProvider interfaces.DataProvider) *Cache {
	return &Cache{
		dataProvider:       dataProvider,
		subAccountBalances: make(map[string][][]string),
		records:            make(map[string][][]string),
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

func (c *Cache) SubAccountBalances(account string) ([][]string, error) {
	if c.subAccountBalances[account] != nil {
		return c.subAccountBalances[account], nil
	}
	data, err := c.dataProvider.SubAccountBalances(account)
	if err != nil {
		return nil, err
	}
	c.subAccountBalances[account] = data
	return data, nil
}

func (c *Cache) Records(account string) ([][]string, error) {
	if c.records[account] != nil {
		return c.records[account], nil
	}
	data, err := c.dataProvider.Records(account)
	if err != nil {
		return nil, err
	}
	c.records[account] = data
	return data, nil
}
