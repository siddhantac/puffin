# Color Schemes and Themes

Puffin now supports multiple color schemes to customize the appearance of your terminal UI.

## Available Color Schemes

### 1. **Dracula** (Currently Active)
A dark theme with vibrant colors:
- Background: Dark purple-gray (#282a36)
- Accent: Bright pink (#ff79c6)
- Text: Light cream (#f8f8f2)

### 2. **Nord**
A clean, arctic-inspired theme:
- Background: Dark blue-gray (#2e3440)
- Accent: Red (#bf616a)
- Text: Light blue-gray (#d8dee9)

### 3. **Gruvbox**
A warm, retro-inspired theme:
- Background: Dark brown (#282828)
- Accent: Bright pink (#ff79c6)
- Text: Light beige (#ebdbb2)

## How to Switch Themes

### Interactive Theme Switching (Recommended)

You can now change themes dynamically while using the application:

1. **Press `Shift+T`** to cycle through available themes
2. The current theme is displayed in the SETTINGS section in the bottom left
3. Available themes cycle in order: **Dracula** → **Gruvbox** → **Nord** → **Dracula**...

### Manual Theme Switching (Code Changes)

Alternatively, to permanently set a specific theme, edit the `ui/style.go` file and modify line 8:

#### For Dracula (current):
```go
var currentTheme ThemeName = ThemeDraculaName
```

#### For Nord:
```go
var currentTheme ThemeName = ThemeNordName
```

#### For Gruvbox:
```go
var currentTheme ThemeName = ThemeGruvboxName
```

After making the change, rebuild the application:
```bash
go build -o puffin .
```

## Color Scheme Files

- `ui/colorscheme/dracula.go` - Dracula color definitions
- `ui/colorscheme/nord.go` - Nord color definitions
- `ui/colorscheme/gruvbox.go` - Gruvbox color definitions
- `ui/theme.go` - Theme configuration functions
- `ui/style.go` - Main styling configuration
- `ui/v2/styles.go` - Version 2 UI styles

## Adding Custom Themes

To add a new theme:

1. Create a new color scheme file in `ui/colorscheme/` (e.g., `mytheme.go`)
2. Define your color constants
3. Add a new theme function in `ui/theme.go`
4. Update the theme variable in `ui/style.go`
5. Rebuild the application

Example color scheme structure:
```go
package colorscheme

const (
    MyThemeBackground = "#123456"
    MyThemeForeground = "#abcdef"
    // ... more colors
)
```
