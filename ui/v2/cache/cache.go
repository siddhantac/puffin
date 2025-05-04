package cache

import (
	"log"
	"puffin/ui/v2/interfaces"
)

// Cache implements a unified generic caching layer for the data provider
type Cache struct {
	dataProvider interfaces.DataProvider

	// A single generic cache
	cache map[string]map[string]any
}

// NewCache creates a new cache instance with initialized maps
func NewCache(dataProvider interfaces.DataProvider) *Cache {
	return &Cache{
		dataProvider: dataProvider,
		cache:        make(map[string]map[string]any),
	}
}

// getOrFetch is a generic function that handles caching logic for any data type
// This is now a standalone function rather than a method on Cache
func getOrFetch[T any](
	cache map[string]map[string]any,
	method string,
	key string,
	fetcher func() (T, error),
) (T, error) {
	log.Printf("cache 1 %v %v", method, key)
	// Ensure the cache map exists for this method
	if cache[method] == nil {
		cache[method] = make(map[string]any)
	}

	// Check if data is in cache
	if value, exists := cache[method][key]; exists {
		// Type assertion with the generic type
		return value.(T), nil
	}
	log.Printf("cache 2 %v %v", method, key)

	// Fetch data if not in cache
	data, err := fetcher()
	if err != nil {
		var zero T
		return zero, err
	}
	log.Printf("cache 3 %v %v", method, key)

	// Store in cache and return
	cache[method][key] = data
	return data, nil
}

// Balance returns cached balance data or fetches it if not in cache
func (c *Cache) Balance(filter interfaces.Filter, displayOptions interfaces.DisplayOptions) ([][]string, error) {
	return getOrFetch(
		c.cache,
		"balance",
		cacheKey(filter),
		func() ([][]string, error) {
			return c.dataProvider.Balance(filter, displayOptions)
		},
	)
}

// Records returns cached records data or fetches it if not in cache
func (c *Cache) Records(filter interfaces.Filter) ([][]string, error) {
	return getOrFetch[[][]string](
		c.cache,
		"records",
		cacheKey(filter),
		func() ([][]string, error) {
			return c.dataProvider.Records(filter)
		},
	)
}

// IncomeStatement returns cached income statement data or fetches it if not in cache
func (c *Cache) IncomeStatement(filter interfaces.Filter, displayOptions interfaces.DisplayOptions) (*interfaces.ComplexTable, error) {
	return getOrFetch[*interfaces.ComplexTable](
		c.cache,
		"incomeStatement",
		cacheKey(filter),
		func() (*interfaces.ComplexTable, error) {
			return c.dataProvider.IncomeStatement(filter, displayOptions)
		},
	)
}

// BalanceSheet returns cached balance sheet data or fetches it if not in cache
func (c *Cache) BalanceSheet(filter interfaces.Filter, displayOptions interfaces.DisplayOptions) (*interfaces.ComplexTable, error) {
	return getOrFetch[*interfaces.ComplexTable](
		c.cache,
		"balanceSheet",
		cacheKey(filter),
		func() (*interfaces.ComplexTable, error) {
			return c.dataProvider.BalanceSheet(filter, displayOptions)
		},
	)
}

// cacheKey creates a unique key for the cache based on filter properties
func cacheKey(filter interfaces.Filter) string {
	return filter.AccountType + filter.Account + filter.DateStart + filter.DateEnd + filter.Description
}
