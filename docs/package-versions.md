# Package Versions — Dendro
# This file is the source of truth for which versions we target.
# Update this when upgrading any package.

## R Version
- **R 4.5.2** (installed at `C:/Program Files/R/R-4.5.2/`)

## Core Packages

| Package       | Pinned Version | Last Verified | Notes |
|---------------|---------------|---------------|-------|
| ellmer        | 0.4.0         | 2026-03-03    | Tool-calling uses `tool()` with named `arguments` list (v0.3.0+ syntax). `chat_azure()` removed — use `chat_azure_openai()`. `api_key` deprecated → use `credentials` or env vars. Dev 0.4.0.9000 has fixes for empty ContentText in tool calling. See `docs/ellmer-api.md`. |
| ragnar        | 0.3.0         | 2026-03-03    | RAG workflows: document loading, chunking, DuckDB vector store, retrieval, ellmer chat integration. `embed_azure_openai()` available. Tool name prefix changed to `search_*`. See `docs/ragnar-api.md`. |
| bslib         | 0.9.0         | 2026-02-25    | Bootstrap 5, no built-in chat component |
| shiny         | 1.12.1        | 2026-02-25    | |
| ggplot2       | 4.0.1         | 2026-03-02    | Chart rendering. Uses `is_ggplot()` (not deprecated `is.ggplot()`). |
| officer       | 0.7.3         | 2026-02-25    | PowerPoint generation |
| haven         | 2.5.5         | 2026-02-25    | SPSS file reading with `read_sav()` |
| readxl        | 1.4.5         | 2026-02-25    | Excel data file reading |
| readr         | 2.1.6         | 2026-03-02    | CSV parsing |
| yaml          | 2.3.12        | 2026-03-02    | Client palette config files. |
| colourpicker  | TBD           | —             | Color picker input for custom palettes. Verify installed version. |

## Additional Dependencies

| Package     | Version | Purpose                          |
|-------------|---------|----------------------------------|
| cli         | 3.6.5   | Console messaging                |
| testthat    | 3.3.2   | Unit testing                     |
| dplyr       | 1.1.4   | Data manipulation                |
| scales      | 1.4.0   | Axis formatting ($, %, comma)    |

## Known Issues
- `officer` requires a working R graphics device to place charts. On headless servers,
  ensure `cairo` or equivalent PNG device is available.
- `colourpicker` requires `shinyjs` or `htmlwidgets` — verify no conflicts with bslib.
- **ellmer `api_key` deprecation:** Resolved. `mod_ai_chat.R` now uses `credentials`
  function for Azure and relies on env var auto-detection for other providers.
  All providers use `echo = "none"` (string) instead of `echo = FALSE` (boolean).

## Breaking Changes Log
- 2026-03-03: Updated ellmer notes — `chat_azure()` permanently removed in 0.4.0,
  must use `chat_azure_openai()`. Added ragnar 0.3.0 to inventory.
- 2026-03-02: Forked from CrossTalk package inventory. Removed gt, openxlsx2, flextable.
  Added ggplot2, readr, yaml, colourpicker, scales.
