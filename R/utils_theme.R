# utils_theme.R
# Centralized theme color constants for R code (ggplot, DT, inline styles)
# Keep in sync with www/styles.css design tokens

# Core palette
THEME_ACCENT         <- "#2563eb"
THEME_TEXT_PRIMARY    <- "#1a1a1a"
THEME_TEXT_SECONDARY  <- "#737373"
THEME_BG_PRIMARY     <- "#ffffff"
THEME_BG_SECONDARY   <- "#fafafa"
THEME_BORDER         <- "#e5e5e5"
THEME_BORDER_LIGHT   <- "#f0f0f0"

# Status colors for DT formatStyle
THEME_STATUS_OK      <- "#dcfce7"
THEME_STATUS_WARNING <- "#fef9c3"
THEME_STATUS_DANGER  <- "#fee2e2"
THEME_STATUS_NEUTRAL <- "#f5f5f5"

# Chart colors for ggplot
THEME_CHART_PRIMARY   <- "#2563eb"
THEME_CHART_SECONDARY <- "#7c3aed"

# Chat bubble colors
THEME_CHAT_USER_BG       <- "#2563eb"
THEME_CHAT_AI_BORDER     <- "#e5e5e5"
THEME_CHAT_AI_LEFT       <- "#93c5fd"
THEME_CHAT_CONTAINER_BG  <- "#fafafa"

# Tree visualization palettes
TREE_PALETTE_CLASSIFICATION <- c(
  "#66c2a5", "#fc8d62", "#8da0cb", "#e78ac3",
  "#a6d854", "#ffd92f", "#e5c494", "#b3b3b3"
)  # Set2 — muted qualitative, 8 colors
TREE_PALETTE_REGRESSION <- "Blues"
TREE_NODE_BORDER <- "#d4d4d4"
