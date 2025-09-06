package ui

import (
	"context"
	"encoding/base64"
	"fmt"
	"log"
	"os"
	"path/filepath"
	"time"

	"github.com/chromedp/chromedp"
)

// renderRevenueChartPNG renders the revenue chart to a PNG via headless Chrome.
// It returns the absolute path to the PNG file.
func renderRevenueChartPNG(rd *revenueData, width, height int) (string, error) {
	html := buildRevenueChartHTML(rd, width, height)
	return renderHTMLChartPNG(html, "puffin_revenue", 15*time.Second)
}

// renderHTMLChartPNG renders arbitrary HTML to a PNG via headless Chrome.
// prefix is used to construct a temp filename.
func renderHTMLChartPNG(html, prefix string, timeout time.Duration) (string, error) {
	ctx, cancel := chromedp.NewContext(context.Background())
	defer cancel()
	ctx, cancel = context.WithTimeout(ctx, timeout)
	defer cancel()

	var pngBuf []byte
	url := "data:text/html;base64," + base64.StdEncoding.EncodeToString([]byte(html))
	// Logging for diagnostics
	log.Printf("chromedp: navigate start (html len=%d)", len(html))
	tasks := chromedp.Tasks{
		chromedp.Navigate(url),
		chromedp.WaitVisible("#chart", chromedp.ByID),
		chromedp.Sleep(800 * time.Millisecond),
		chromedp.FullScreenshot(&pngBuf, 100),
	}
	if err := chromedp.Run(ctx, tasks); err != nil {
		log.Printf("chromedp: error: %v", err)
		return "", err
	}
	log.Printf("chromedp: screenshot bytes=%d", len(pngBuf))

	dir := os.TempDir()
	file := filepath.Join(dir, fmt.Sprintf("%s_%d.png", prefix, time.Now().UnixNano()))
	if err := os.WriteFile(file, pngBuf, 0o644); err != nil {
		log.Printf("chromedp: write error: %v", err)
		return "", err
	}
	log.Printf("chromedp: wrote %s", file)
	return file, nil
}

