# Dendro Enhancement Specification

## Overview

Systematic enhancement of the Dendro R Shiny decision tree application to make it production-ready. The app serves a mixed audience (internal team + client-facing) and deploys on Posit Connect.

---

## 1. Architecture: Modularization

**Goal:** Split the monolithic 3,200-line `app.R` into Shiny modules.

**Module structure:**
```
Dendro/
├── app.R                    # Entry point: loads modules, assembles UI/server
├── R/
│   ├── mod_data_import.R    # File upload, demo data selection, data dictionary
│   ├── mod_data_quality.R   # Quality checks, outlier detection, recommendations
│   ├── mod_factor_editor.R  # Factor level reordering and management
│   ├── mod_tree_config.R    # Model parameters (cp, minbucket, maxdepth)
│   ├── mod_tree_viz.R       # Static rpart.plot + interactive visNetwork tree
│   ├── mod_model_details.R  # Stats, importance, confusion matrix, RF comparison
│   ├── mod_ai_chat.R        # AI provider setup, personas, chat interface
│   ├── mod_export.R         # PowerPoint, HTML, PDF, image exports
│   ├── mod_guide.R          # Help accordion and glossary
│   ├── utils_data.R         # Data processing helpers (type conversion, etc.)
│   ├── utils_model.R        # Model building, importance extraction
│   ├── utils_theme.R        # Theme definition and design tokens
│   └── utils_demo_data.R    # Demo dataset definitions
├── www/
│   └── styles.css           # Rewritten for new theme
├── tests/
│   ├── testthat/
│   │   ├── test-utils_data.R
│   │   ├── test-utils_model.R
│   │   └── test-mod_data_quality.R
│   └── testthat.R
└── dendro.Rproj
```

**Conventions:**
- Each `mod_*.R` file exports a `*_ui(id)` and `*_server(id, ...)` function pair
- Modules communicate via returned reactive values, not global state
- `utils_*.R` files contain pure functions (no Shiny reactivity) — testable independently
- `app.R` is the orchestrator: wires modules together, passes shared reactives

---

## 2. Tree Visualization Overhaul

### 2a. Static Tree (rpart.plot) — Fix Ugly Trees

**Current problem:** Trees use a single BuGn palette, look bland and unclear.

**Requirements:**
- **Classification models:** Each terminal node class gets a distinct, clearly separated color. Use a qualitative palette (e.g., Set2, Paired, or custom muted palette) so groups are instantly distinguishable
- **Regression models:** Use a sequential palette (e.g., blues or teals) based on predicted value
- **General aesthetics:**
  - Clean, modern typography (no default serif)
  - Rounded boxes with subtle borders
  - Clear node labels: show class, probability/percentage, and n
  - Adequate spacing — not cramped
  - White or very light background
  - Shadow or depth cues to separate nodes visually
- **Configurable:** Let users adjust `extra` parameter or toggle between compact/detailed views
- **Export-friendly:** High-resolution output for PowerPoint and image export

### 2b. Interactive Tree (visNetwork)

**New feature:** Add a visNetwork-based interactive tree view alongside the static plot.

**Requirements:**
- Tab or toggle to switch between static and interactive views
- Nodes colored by class (matching the static plot palette)
- Click a node to see: split rule, n, class distribution, probability
- Zoom, pan, collapse/expand branches
- Tooltip or sidebar panel for node details
- Responsive sizing within the app layout

---

## 3. AI Prompt & Response Improvements

### 3a. Persona Prompt Adjustments

**Executive Summary persona:**
- Strictly interpretation only — do NOT suggest next steps, additional analyses, or methodological changes
- Focus on: what the tree found, what it means for the business/program
- Bullet-point format, key takeaways first

**Project Team persona:**
- Strictly interpretation only — do NOT suggest running more analyses or parameter changes
- Focus on: findings, implications, actionable insights from the existing model
- Report-ready language, structured with headers and bullets

**Statistician persona:**
- MAY suggest next steps: parameter tuning, variables to explore, validation approaches
- Full technical detail with proper terminology
- Can recommend methodological improvements

### 3b. Response Formatting

**All personas should:**
- Lead with key takeaways as bullet points
- Avoid long paragraphs — break into short, scannable sections
- Use markdown headers and bullet lists extensively
- When referencing numbers, embed them inline in context (not raw dumps of statistics)
- Bold important findings
- Keep responses concise — quality over quantity
- Use the data dictionary labels (not raw variable names) when available

---

## 4. Data Enhancements

### 4a. Multiple Demo Datasets

**Replace single hardcoded demo with a selector offering 2-3 healthcare datasets:**

Options (all synthetic/built-in, no external dependencies):
1. **HCP Prescribing Survey** — Current dataset (500 obs, 23 vars). Classification use case.
2. **Patient Readmission** — Synthetic hospital readmission data. Binary classification.
3. **Treatment Response** — Synthetic clinical trial response data. Could support regression use case.

Each demo should include:
- A matching data dictionary with variable labels
- A brief description shown in the UI when selected

### 4b. Outlier Detection

**Add to the Data Quality module:**

- Detect numeric outliers using IQR method (1.5x and 3x thresholds)
- Show count and percentage of outliers per variable
- Flag extreme outliers separately from mild outliers
- Visual indicator (e.g., mini box plot or severity badge)
- Recommendation text (e.g., "Variable X has 5% extreme outliers — consider reviewing")
- No automatic removal — informational only, user decides

---

## 5. Export Expansion

**Add alongside existing PowerPoint export:**

- **HTML report:** Self-contained HTML file with embedded tree plot, importance chart, model stats, and AI interpretation (if available). Use `htmltools` or `rmarkdown::render`.
- **PDF report:** Same content as HTML but rendered to PDF. Use `webshot2` or `chrome_print`.
- **Image export:** Download tree plot and importance chart as PNG or SVG individually.

**Export UI:** Replace single download button with a dropdown or button group offering format choices.

---

## 6. Theme Rebrand — Modern Minimal

**Direction:** Clean, minimal, professional. Inspired by Linear/Notion/Vercel aesthetic.

**Design tokens:**
- **Primary color:** Single accent color (slate blue, teal, or similar — not indigo gradient)
- **Background:** White (`#ffffff`) with subtle gray (`#f8f9fa`) section backgrounds
- **Text:** Near-black (`#111827`) for body, medium gray (`#6b7280`) for secondary
- **Borders:** Light gray (`#e5e7eb`), 1px, subtle
- **Border radius:** Small — 6px for cards, 4px for inputs
- **Shadows:** Minimal — only on elevated elements (modals, dropdowns), very subtle
- **Typography:** Inter or system font stack, clear hierarchy
- **Spacing:** Generous whitespace, not cramped

**Component style:**
- Cards: White background, thin border, no heavy shadows or hover lift
- Buttons: Solid accent color (primary) or ghost/outline (secondary). No gradients.
- Value boxes: Clean, flat, single accent color left border — no pastel gradients
- Tables: Clean headers (not gradient), subtle row striping, minimal borders
- Chat bubbles: Simple left-aligned for AI, right-aligned for user, subtle background difference
- Sidebar: Clean, well-spaced, collapsible sections without heavy accordion styling
- Badges: Simple, flat, small

**Remove:**
- Gradient backgrounds on buttons, headers, badges
- Heavy box shadows with color tints
- Bounce/lift hover animations
- Decorative elements that don't serve function

---

## 7. Testing

**Framework:** `testthat` for unit tests, `shinytest2` for integration tests.

**Unit tests (`testthat`):**
- `utils_data.R`: type conversion, factor handling, prefix detection
- `utils_model.R`: model building, importance extraction, confusion matrix generation
- `mod_data_quality.R` logic: outlier detection, missing data checks, variance checks
- Demo data integrity: correct dimensions, expected columns, no NA in key fields

**Integration tests (`shinytest2`):**
- App launches without error
- Demo data loads and populates UI
- Model builds with default parameters
- Tab navigation works
- Export produces a file

**Goal:** Catch regressions when refactoring. Not 100% coverage — focus on core logic and happy paths.

---

## 8. Production / Dev Mode

**Goal:** Enforce data security rules around AI provider usage. Replaces the existing simple `PRODUCTION_MODE` boolean with smarter logic.

**Rules:**
- **Production mode (`PRODUCTION_MODE = TRUE`):**
  - AI chat restricted to Azure OpenAI only (secure, enterprise-compliant)
  - Non-Azure providers are hidden from the provider dropdown entirely
  - Badge in UI: "PRODUCTION" — visible but unobtrusive
  - Intended for client data and real analyses

- **Dev mode (`PRODUCTION_MODE = FALSE`):**
  - All AI providers available (Azure, Claude, Gemini, GPT, Ollama)
  - **Non-Azure providers only enabled when using a demo dataset** — if the user uploads their own data and selects a non-Azure provider, show a warning and block the AI chat: "Non-Azure AI providers are only available with demo datasets. Switch to Azure OpenAI for uploaded data."
  - Badge in UI: "DEV"
  - Intended for testing, demos, and proof-of-concept

**Implementation:**
- Reactive guard in the AI chat module: check `is_demo_data` flag + selected provider
- When user switches from demo to uploaded data, auto-switch provider to Azure if currently on a non-Azure provider (with a notification explaining why)
- When user selects a non-Azure provider while using uploaded data, show inline warning and disable the send button
- Keep `PRODUCTION_MODE` as a top-level flag in `app.R` (or environment variable)

---

## 9. Deployment Considerations (Posit Connect)

- Ensure all dependencies are CRAN-available or documented
- Add `renv` for reproducible package management
- `.Renviron` for API keys (already in place)
- `PRODUCTION_MODE` set via environment variable on Posit Connect
- No session persistence needed — each session is fresh

---

## 10. Out of Scope (Explicit)

These were considered and deferred:
- Session save/load
- Accessibility / internationalization (not priority now)
- Additional model types beyond rpart
- Cross-validation / pruning optimization
- AI-driven analysis suggestions (except Statistician persona)
- Dark mode
- Custom/configurable themes per deployment
- Configurable AI endpoints

---

## Implementation Order (Suggested)

1. **Modularize** — Split `app.R` into modules (foundation for everything else)
2. **Theme rebrand** — New CSS and design tokens (affects all UI work downstream)
3. **Production/Dev mode** — Enforce AI provider rules based on data source
4. **Tree visualization** — Fix rpart.plot aesthetics + add visNetwork
5. **AI prompts & formatting** — Update persona prompts and response formatting
6. **Data enhancements** — Demo dataset selector + outlier detection
7. **Export expansion** — Add HTML, PDF, image exports
8. **Testing** — Add tests for core logic and integration
9. **Polish & deploy** — Final QA, renv setup, Posit Connect deployment
