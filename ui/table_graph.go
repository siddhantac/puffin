package ui

import (
	"strconv"

	"github.com/charmbracelet/bubbles/table"
	"github.com/charmbracelet/bubbles/viewport"
	tea "github.com/charmbracelet/bubbletea"
	"github.com/charmbracelet/lipgloss"
	"github.com/guptarohit/asciigraph"
)

var tableData = []table.Row{
	{"account", "commodity", "2021-12", "2022-01", "2022-02", "2022-03", "2022-04", "2022-05"},
	{"assets:bank:chase", "$", "-800.00", "10000.00", "0", "0", "0", "-3851.65"},
	{"assets:bank:maybank", "$", "-2300.00", "-10000.00", "0", "0", "0", "0"},
	{"assets:bank:stanchart", "$", "0.05", "22.54", "0", "0", "0", "0"},
	{"assets:invest:crypto", "$", "0", "0", "0", "0", "0", "2000.00"},
	{"assets:invest:stocks", "$", "0", "0", "0", "0", "0", "1000.00"},
	{"expenses:entertainment", "$", "0", "0", "0", "0", "0", "21.98"},
	{"expenses:fitness", "$", "60.00", "0", "0", "0", "0", "0"},
	{"expenses:groceries", "$", "0", "-4.88", "0", "0", "0", "8.36"},
	{"expenses:household", "$", "800.00", "0", "0", "0", "0", "0"},
	{"expenses:rent", "$", "2300.00", "0", "0", "0", "0", "0"},
	{"expenses:travel", "$", "0", "0", "0", "0", "0", "15.05"},
	{"expenses:utilities", "$", "100.54", "39.99", "0", "0", "0", "59.99"},
	{"income:interest", "$", "0", "0", "0", "0", "0", "-22.89"},
	{"income:others", "$", "-0.05", "0", "0", "0", "0", "0"},
	{"liabilities:credit_card:american_express", "$", "-160.54", "-14.67", "0", "0", "0", "690.07"},
	{"liabilities:credit_card:citibank", "$", "0", "-42.98", "0", "0", "0", "79.09"},
	{"total", "", "0", "0", "0", "0", "0", "0"},
}

func (t *TableGraph) IsReady() bool { return true }
func (t *TableGraph) SetUnready()   {}
func (t *TableGraph) SetContent(tea.Msg) {
	t.table.SetContent(newGenericTableData(tableData))

	row := t.table.SelectedRow()
	t.viewport.SetContent(getGraph(strSliceToNumbers(row[2:])))
}

func strSliceToNumbers(s []string) []float64 {
	var numbers []float64
	for _, v := range s {
		n, _ := strconv.ParseFloat(v, 64)
		numbers = append(numbers, n)
	}
	return numbers
}

func getGraph(rows []float64) string {
	graph := asciigraph.Plot(
		rows,
		asciigraph.SeriesColors(asciigraph.Red),
		asciigraph.Height(20),
		asciigraph.Width(80),
	)
	return graph
}

type TableGraph struct {
	table    *Table
	viewport viewport.Model
}

func newTableGraph() *TableGraph {
	return &TableGraph{
		table:    newTable(nil),
		viewport: viewport.New(10, 10),
	}
}

func (t *TableGraph) Init() tea.Cmd {
	return nil
}

func (t *TableGraph) Update(msg tea.Msg) (tea.Model, tea.Cmd) {
	t.table.Update(msg)
	t.viewport.Update(msg)

	switch msg := msg.(type) {
	case tea.WindowSizeMsg:
		headerHeight := lipgloss.Height(header())
		verticalMarginHeight := headerHeight + footerHeight
		tableHeight := (msg.Height - verticalMarginHeight - 3) / 2
		t.table.SetHeight(tableHeight)

		t.viewport = viewport.New(msg.Width, tableHeight-2)
		t.viewport.YPosition = tableHeight
	case tea.KeyMsg:
		switch msg.String() {
		case "J", "K":
			row := t.table.SelectedRow()
			t.viewport.SetContent(getGraph(strSliceToNumbers(row)))
		}
	}

	return t, nil
}

func (t *TableGraph) View() string {
	s := lipgloss.NewStyle().
		BorderStyle(lipgloss.NormalBorder()).
		BorderForeground(lipgloss.Color("240")).
		BorderBottom(true)

	return lipgloss.JoinVertical(lipgloss.Left, s.Render(t.table.View()), t.viewport.View())
}
