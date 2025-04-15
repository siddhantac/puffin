package ui

import (
	"log"
	"puffin/ui/v2/interfaces"

	"github.com/charmbracelet/bubbles/table"
	"github.com/charmbracelet/bubbles/viewport"
	tea "github.com/charmbracelet/bubbletea"
	"github.com/charmbracelet/lipgloss"
)

var (
	activeTitleStyle   = lipgloss.NewStyle().Bold(true).Background(lipgloss.Color("57")).PaddingLeft(1).PaddingRight(1)
	inactiveTitleStyle = lipgloss.NewStyle().Bold(true).PaddingLeft(1).PaddingRight(1)
)

func tableStyle() table.Styles {
	s := table.DefaultStyles()
	s.Header = s.Header.
		BorderStyle(lipgloss.NormalBorder()).
		BorderForeground(lipgloss.Color("240")).
		BorderBottom(true).
		Bold(false)
	s.Selected = s.Selected.
		Foreground(lipgloss.Color("229")).
		Background(lipgloss.Color("57")).
		Bold(false)
	return s
}

func tblStyleActive() lipgloss.Style {
	return lipgloss.NewStyle().Border(lipgloss.RoundedBorder()).BorderForeground(lipgloss.Color("White"))
}

func tblStyleInactive() lipgloss.Style {
	return lipgloss.NewStyle().Border(lipgloss.RoundedBorder()).BorderForeground(lipgloss.Color("240"))
}

type updateIncomeStatement struct{}

type complexTable struct {
	title, upperTitle, lowerTitle string
	bottomBar                     table.Model
	upper, lower                  table.Model
	upperFocused                  bool
}

func newComplexTable() *complexTable {
	return &complexTable{
		upper: table.New(),
		lower: table.New(),
		// upperFocused: true,
	}
}

func (c *complexTable) Init() tea.Cmd {
	c.upper.Focus()
	c.lower.Blur()
	return nil
}

func (c *complexTable) Update(msg tea.Msg) (*complexTable, tea.Cmd) {
	var cmd tea.Cmd

	switch msg := msg.(type) {
	case tea.KeyMsg:
		switch msg.String() {
		case "]", "[":
			if c.upper.Focused() {
				c.upper.Blur()
				c.lower.Focus()
			} else {
				c.upper.Focus()
				c.lower.Blur()
			}
		default:
			if c.upper.Focused() {
				c.upper, cmd = c.upper.Update(msg)
			} else {
				c.lower, cmd = c.lower.Update(msg)
			}
		}
	}
	return c, cmd
}

func (c *complexTable) View() string {
	tblStyle := tableStyle()
	c.upper.SetStyles(tblStyle)
	c.lower.SetStyles(tblStyle)

	var upperStyle, lowerStyle lipgloss.Style

	if c.upper.Focused() {
		upperStyle = tblStyleActive()
		lowerStyle = tblStyleInactive()
	} else {
		upperStyle = tblStyleInactive()
		lowerStyle = tblStyleActive()
	}
	return lipgloss.JoinVertical(lipgloss.Left,
		lipgloss.NewStyle().Bold(true).Render(c.title),
		c.upperTitle,
		upperStyle.Render(c.upper.View()),
		c.lowerTitle,
		lowerStyle.Render(c.lower.View()),
	)
}

type advancedReports struct {
	incomeStatement2  *complexTable
	incomeStatement   viewport.Model
	balanceSheet      viewport.Model
	dataProvider      interfaces.DataProvider
	filterGroup       *filterGroup
	focusedModel      viewport.Model
	focusedModelTitle string
	height, width     int
}

func newAdvancedReports(dataProvider interfaces.DataProvider) *advancedReports {
	return &advancedReports{
		incomeStatement2: newComplexTable(),
		incomeStatement:  viewport.New(0, 0),
		balanceSheet:     viewport.New(0, 0),
		dataProvider:     dataProvider,
		filterGroup:      newFilterGroupAdvReports(),
	}
}

func (a *advancedReports) Init() tea.Cmd {
	return a.incomeStatement2.Init()
}

func (a *advancedReports) Update(msg tea.Msg) (tea.Model, tea.Cmd) {
	log.Printf("adv repo: msg: %T | %v", msg, msg)
	var cmd tea.Cmd
	switch msg := msg.(type) {
	case tea.WindowSizeMsg:
		a.height = msg.Height
		a.width = msg.Width

		a.incomeStatement = viewport.New(msg.Width, percent(msg.Height, 88))
		a.setIncomeStatementData()
		a.focusedModel = a.incomeStatement
		a.focusedModelTitle = lipgloss.JoinHorizontal(
			lipgloss.Top,
			activeTitleStyle.Render("(1) Income Statement"),
			inactiveTitleStyle.Render("(2) Balance Sheet"),
		)

		a.balanceSheet = viewport.New(msg.Width, percent(msg.Height, 88))
		a.setBalanceSheetData()

		fg, cmd := a.filterGroup.Update(msg)
		a.filterGroup = fg.(*filterGroup)

		a.incomeStatement2.upper.SetHeight((msg.Height - 18) / 2)
		a.incomeStatement2.lower.SetHeight((msg.Height - 18) / 2)

		return a, tea.Sequence(
			a.updateIncomeStatementCmd,
			cmd,
		)

	case tea.KeyMsg:
		if a.filterGroup.Focused() {
			if msg.String() == "esc" {
				a.filterGroup.Blur()
				return a, nil
			}
			if msg.String() == "enter" {
				a.filterGroup.Blur()
				return a, a.updateIncomeStatementCmd
			}

			fg, cmd := a.filterGroup.Update(msg)
			a.filterGroup = fg.(*filterGroup)
			return a, cmd
		}
		switch msg.String() {
		case "f":
			a.filterGroup.Focus()
			return a, nil
		case "1":
			a.focusedModel = a.incomeStatement
			a.focusedModelTitle = lipgloss.JoinHorizontal(
				lipgloss.Top,
				activeTitleStyle.Render("(1) Income Statement"),
				inactiveTitleStyle.Render("(2) Balance Sheet"),
			)
		case "2":
			a.focusedModel = a.balanceSheet
			a.focusedModelTitle = lipgloss.JoinHorizontal(
				lipgloss.Top,
				inactiveTitleStyle.Render("(1) Income Statement"),
				activeTitleStyle.Render("(2) Balance Sheet"),
			)
		}

	case updateIncomeStatement:
		log.Printf("income statement update")
		a.setIncomeStatementData()
		a.setBalanceSheetData()
	}
	a.incomeStatement, cmd = a.incomeStatement.Update(msg)
	a.balanceSheet, cmd = a.balanceSheet.Update(msg)
	a.incomeStatement2, cmd = a.incomeStatement2.Update(msg)

	return a, cmd
}

func (a *advancedReports) updateIncomeStatementCmd() tea.Msg {
	return updateIncomeStatement{}
}

func (a *advancedReports) setIncomeStatementData() {
	filter := interfaces.Filter{
		Account:     a.filterGroup.AccountName(),
		DateStart:   a.filterGroup.DateStart(),
		DateEnd:     a.filterGroup.DateEnd(),
		Description: a.filterGroup.Description(),
	}
	// data, err := a.dataProvider.IncomeStatement(filter)
	// if err != nil {
	// 	log.Printf("error: %v", err)
	// 	a.incomeStatement.SetContent(err.Error())
	// 	return
	// }
	// a.incomeStatement.SetContent(data)

	complexTable, err := a.dataProvider.IncomeStatement2(filter)
	if err != nil {
		log.Printf("error: %v", err)
		a.incomeStatement.SetContent(err.Error())
		return
	}
	a.incomeStatement2.title = complexTable.Title
	a.incomeStatement2.upperTitle = complexTable.UpperTitle
	a.incomeStatement2.lowerTitle = complexTable.LowerTitle

	accountColWidth := percent(a.width, 20)
	commodityColWidth := 10
	remainingWidth := a.width - accountColWidth - commodityColWidth - 2
	otherColumnsWidth := remainingWidth/(len(complexTable.Columns)-2) - 2

	cols := []table.Column{
		{
			Title: complexTable.Columns[0],
			Width: accountColWidth,
		},
		{
			Title: complexTable.Columns[1],
			Width: commodityColWidth,
		},
	}
	for _, c := range complexTable.Columns[2:] {
		cols = append(cols,
			table.Column{
				Title: c,
				Width: otherColumnsWidth,
			})
	}
	a.incomeStatement2.upper.SetColumns(cols)
	a.incomeStatement2.lower.SetColumns(cols)

	upperRows := make([]table.Row, 0, len(complexTable.Upper))
	for _, row := range complexTable.Upper {
		upperRows = append(upperRows, row)
	}
	a.incomeStatement2.upper.SetRows(upperRows)

	lowerRows := make([]table.Row, 0, len(complexTable.Upper))
	for _, row := range complexTable.Lower {
		lowerRows = append(lowerRows, row)
	}
	a.incomeStatement2.lower.SetRows(lowerRows)

	// a.incomeStatement2.bottomBar.SetColumns(table.Column(complexTable.BottomBar))
}

func (a *advancedReports) setBalanceSheetData() {
	filter := interfaces.Filter{
		Account:     a.filterGroup.AccountName(),
		DateStart:   a.filterGroup.DateStart(),
		DateEnd:     a.filterGroup.DateEnd(),
		Description: a.filterGroup.Description(),
	}
	data, err := a.dataProvider.BalanceSheet(filter)
	if err != nil {
		return
	}
	a.balanceSheet.SetContent(string(data))
}

func (a *advancedReports) View() string {

	// s := lipgloss.NewStyle().PaddingLeft(2)
	return lipgloss.JoinVertical(
		lipgloss.Left,
		a.filterGroup.View(),
		lipgloss.JoinVertical(
			lipgloss.Left,
			a.focusedModelTitle,
			a.incomeStatement2.View(),
		),
	)
}
