# bslib API Reference (version: 0.9.0)
# Last updated: 2026-02-25
# Source: https://rstudio.github.io/bslib/

**Note:** bslib 0.9.0 does NOT include a built-in chat UI component. For chat UI, you'll need to build a custom module using standard Shiny inputs/outputs or use a separate package.

## Page Layout

### `page_sidebar()` — Dashboard with sidebar

```r
library(shiny)
library(bslib)

ui <- page_sidebar(
  title = "CrossTalk",
  sidebar = sidebar(
    # sidebar contents here
  ),
  # main content area
  card(
    card_header("Output"),
    card_body(
      # outputs here
    )
  ),
  fillable = TRUE,            # Default TRUE — main area is flexbox
  fillable_mobile = FALSE,    # Default FALSE
  theme = bs_theme(),
  window_title = NA,          # Defaults to title text
  lang = NULL
)
```

### `page_navbar()` — Multi-page with top nav

```r
ui <- page_navbar(
  title = "CrossTalk",
  id = "nav",
  sidebar = sidebar(...),     # Shared sidebar on all pages
  nav_panel("Upload", upload_ui),
  nav_panel("Analysis", analysis_ui),
  nav_spacer(),
  nav_item(input_dark_mode(id = "dark_mode")),
  fillable = TRUE,
  theme = bs_theme(),
  navbar_options = navbar_options(
    position = "fixed-top",
    bg = NULL,
    collapsible = TRUE
  )
)
```

### `page_fillable()` — Full-height page

```r
ui <- page_fillable(
  card("Content fills the page"),
  theme = bs_theme(),
  fillable_mobile = FALSE
)
```

## Sidebar

### `sidebar()` — Create a sidebar

```r
sidebar(
  # UI elements (inputs, text, etc.)
  fileInput("file", "Upload data"),
  selectInput("weight", "Weight variable", choices = NULL),
  width = 250,                            # CSS unit, default 250px
  position = c("left", "right"),          # Default "left"
  open = NULL,                            # See open options below
  id = NULL,                              # For reactive read/toggle
  title = NULL,                           # Optional header
  bg = NULL,                              # Background color
  fg = NULL,                              # Foreground color
  class = NULL,
  gap = NULL,                             # Spacing between children
  padding = NULL
)
```

**`open` options:**
- `"desktop"` (default): Open on desktop, closed on mobile
- `"open"` or `TRUE`: Always starts open
- `"closed"` or `FALSE`: Always starts closed
- `"always"` or `NA`: Always open, cannot close
- `list(desktop = "open", mobile = "closed")`: Set per-device

### `toggle_sidebar()` — Programmatic toggle

```r
# In server:
toggle_sidebar("sidebar_id")              # Toggle
toggle_sidebar("sidebar_id", open = TRUE) # Force open
```

### `layout_sidebar()` — Sidebar inside a card

```r
card(
  layout_sidebar(
    sidebar = sidebar("Sidebar inside card"),
    "Main content of card",
    fillable = TRUE,
    fill = TRUE,
    border = NULL,
    border_radius = NULL
  )
)
```

## Cards

### `card()` — Container with border

```r
card(
  full_screen = FALSE,        # Show expand icon on hover
  height = NULL,              # Any CSS unit
  max_height = NULL,
  min_height = NULL,
  fill = TRUE,                # Grow/shrink in fillable container
  class = NULL,
  wrapper = card_body,        # Wraps non-card-item children
  id = NULL                   # For full_screen state: input${id}_full_screen
)
```

### `card_header()`, `card_body()`, `card_footer()`

```r
card(
  card_header("Analysis Results", class = "bg-primary text-white"),
  card_body(
    plotOutput("my_plot"),
    fillable = TRUE,          # Flexbox container
    fill = TRUE,              # Grow/shrink
    min_height = NULL,
    max_height = NULL,
    padding = NULL,           # CSS units or numeric (px)
    gap = NULL
  ),
  card_footer("Source: Survey data")
)
```

### `card_title()`

```r
card(
  card_title("My Card Title"),  # Uses h5 by default
  "Card content"
)
```

### `card_image()`

```r
card(
  card_image(file = "www/logo.png", alt = "Logo"),
  card_body("Content below image")
)
```

## Layout Functions

### `layout_columns()` — 12-column responsive grid

```r
layout_columns(
  card("Column 1"),
  card("Column 2"),
  card("Column 3"),
  col_widths = c(4, 4, 4),   # Must sum to 12 per row; NA = auto
  row_heights = NULL,          # Numeric (fr) or CSS units
  fill = TRUE,
  fillable = TRUE,
  gap = NULL,                  # CSS length for spacing
  height = NULL
)
```

**`col_widths` patterns:**
```r
col_widths = NA                         # Auto-size
col_widths = c(4, 8)                    # 1/3 + 2/3
col_widths = c(6, 6, 12)               # Two cols, then full-width
col_widths = c(-2, 8, -2)              # Negative = empty space
col_widths = breakpoints(              # Responsive
  sm = c(12, 12),                      # Stack on small screens
  md = c(6, 6),                        # Side-by-side on medium
  lg = c(4, 8)                         # Asymmetric on large
)
```

### `layout_column_wrap()` — Uniform grid

```r
layout_column_wrap(
  card("A"), card("B"), card("C"),
  width = "200px",            # Minimum width per column (or 1/N for fixed N)
  fixed_width = FALSE,        # TRUE = exact width; FALSE = minimum width
  heights_equal = "all",      # "all" | "row"
  fill = TRUE,
  fillable = TRUE,
  gap = NULL
)
```

**Width patterns:**
```r
width = 1/2                   # Always 2 columns
width = 1/3                   # Always 3 columns
width = "300px"               # At least 300px per column, wraps as needed
```

## Accordion

### `accordion()` — Collapsible panels

```r
accordion(
  accordion_panel(
    title = "Analysis Settings",
    selectInput("sig", "Significance Level", c("90%", "95%", "99%")),
    value = "settings",       # Unique ID
    icon = NULL
  ),
  accordion_panel(
    title = "Export Options",
    downloadButton("dl_excel", "Excel"),
    value = "export"
  ),
  id = "my_accordion",       # input$my_accordion returns open panel values
  open = NULL,                # NULL = first panel; TRUE = all; FALSE = none
  multiple = TRUE             # Allow multiple open panels
)
```

### Programmatic accordion control (server-side)

```r
accordion_panel_open("my_accordion", values = "settings")
accordion_panel_close("my_accordion", values = "settings")
accordion_panel_set("my_accordion", values = c("settings", "export"))
accordion_panel_insert("my_accordion", accordion_panel("New", "content"), target = "settings")
accordion_panel_remove("my_accordion", target = "settings")
```

## Value Box

```r
value_box(
  title = "Total Respondents",
  value = textOutput("n_resp"),
  showcase = bsicons::bs_icon("people-fill"),
  showcase_layout = "left center",  # "left center" | "top right" | "bottom"
  full_screen = FALSE,
  theme = "primary",                # Named theme or value_box_theme()
  height = NULL
)
```

**Theme options:** `"primary"`, `"secondary"`, `"success"`, `"danger"`, `"warning"`, `"info"`, or literal colors: `"blue"`, `"purple"`, `"pink"`, `"red"`, `"orange"`, `"yellow"`, `"green"`, `"teal"`, `"cyan"`. Prefix with `"bg-"` for background, `"text-"` for foreground, or `"bg-gradient-{from}-{to}"` for gradients.

## Theming

### `bs_theme()` — Create/customize Bootstrap 5 theme

```r
theme <- bs_theme(
  version = 5,                # Bootstrap version (default 5)
  preset = "shiny",           # "shiny" (default for BS5), "bootstrap", or bootswatch name
  bg = "#FFFFFF",             # Background color
  fg = "#1a1a2e",             # Foreground/text color
  primary = "#0062cc",        # Primary accent color
  secondary = "#6c757d",
  success = "#28a745",
  info = "#17a2b8",
  warning = "#ffc107",
  danger = "#dc3545",
  base_font = NULL,           # font_google(), font_face(), or string
  code_font = NULL,
  heading_font = NULL,
  font_scale = NULL,          # Multiplier (e.g., 1.2 = 120%)
  brand = NULL                # Path to _brand.yml or FALSE to disable
)
```

### Font helpers

```r
bs_theme(
  base_font = font_google("Inter"),
  code_font = font_google("Fira Code"),
  heading_font = font_collection("Helvetica Neue", "Arial", "sans-serif")
)
```

### Update an existing theme

```r
theme <- bs_theme_update(theme, primary = "#FF5733")
theme <- bs_add_rules(theme, ".my-class { color: red; }")
theme <- bs_add_variables(theme, "my-var" = "10px")
```

## Dark Mode Toggle

```r
# In UI:
input_dark_mode(id = "dark_mode", mode = NULL)  # NULL = follow system pref

# In server:
observe({
  cat("Current mode:", input$dark_mode, "\n")  # "light" or "dark"
})

# Programmatic toggle:
toggle_dark_mode(mode = "dark")
```

## Nav Panels (for page_navbar)

```r
nav_panel(title = "Tab Name", value = "tab_id", icon = NULL,
  # UI content for this tab
)
nav_spacer()                          # Push items to the right
nav_item(actionLink("help", "Help"))  # Arbitrary nav item
nav_menu(                             # Dropdown menu
  title = "More",
  nav_panel("Sub 1", p("content")),
  nav_panel("Sub 2", p("content"))
)
```

## Complete Minimal Example

```r
library(shiny)
library(bslib)

ui <- page_sidebar(
  title = "CrossTalk",
  theme = bs_theme(preset = "shiny", primary = "#0062cc"),
  sidebar = sidebar(
    title = "Data",
    width = 300,
    fileInput("file", "Upload .sav file", accept = ".sav"),
    selectInput("weight", "Weight variable", choices = c("None" = "")),
    accordion(
      accordion_panel("Settings",
        selectInput("sig_level", "Significance", c("90%" = "0.90", "95%" = "0.95", "99%" = "0.99"), selected = "0.95")
      ),
      open = FALSE
    )
  ),
  layout_columns(
    col_widths = c(4, 8),
    card(
      card_header("Chat"),
      card_body(
        uiOutput("chat_history"),
        textInput("user_input", NULL, placeholder = "Ask about your data..."),
        actionButton("send", "Send", class = "btn-primary w-100")
      )
    ),
    card(
      full_screen = TRUE,
      card_header("Results"),
      card_body(
        uiOutput("table_output")
      ),
      card_footer(
        downloadButton("dl_excel", "Excel"),
        downloadButton("dl_pptx", "PowerPoint")
      )
    )
  )
)

server <- function(input, output, session) {
  # server logic here
}

shinyApp(ui, server)
```

## Known Gotchas for v0.9.0

- **No built-in chat UI component.** You need to build chat UI manually with standard Shiny inputs/outputs or use a third-party package.
- **`page_sidebar()` is the recommended dashboard layout.** It handles responsive behavior, sidebar collapse, and fillable content automatically.
- **`card_body(fillable = TRUE, fill = TRUE)`** is needed for content to fill available space. Both default to `TRUE`.
- **`layout_columns()` vs `layout_column_wrap()`:** Use `layout_columns()` for precise 12-column grid control. Use `layout_column_wrap()` for uniform, auto-wrapping grids.
- **Navbar options moved:** In `page_navbar()`, use `navbar_options = navbar_options(...)` instead of direct `bg`, `position`, `inverse` arguments (those are deprecated).
- **`preset = "shiny"` is the default** when using Bootstrap 5. Use `preset = "bootstrap"` to remove the Shiny-specific styling.
- **gt tables in Shiny:** Use `gt::render_gt()` / `gt::gt_output()` for rendering gt tables. Inside modules, ensure proper NS handling.
