# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Repository Overview

**Dendro** — An R Shiny application for interactive decision tree analysis with AI-powered interpretation. Serves both internal teams and client-facing use cases. Deploys on Posit Connect.

See `SPEC.md` for the full enhancement specification.

## Running the Application

```r
# From the Dendro directory
shiny::runApp()

# Or open dendro.Rproj in RStudio and click "Run App"
```

## Required R Packages

```r
# Core
install.packages(c("shiny", "bslib", "rpart", "rpart.plot", "ellmer",
                    "DT", "dplyr", "tidyr", "ggplot2", "randomForest"))

# Data import
install.packages(c("readxl", "haven"))

# Interactive tree
install.packages("visNetwork")

# Export
install.packages(c("officer", "rvg", "htmltools", "webshot2"))

# Testing
install.packages(c("testthat", "shinytest2"))

# Package management
install.packages("renv")
```

## Environment Configuration

API keys for AI providers should be set in `.Renviron`:
- `ANTHROPIC_API_KEY` — Claude
- `GOOGLE_API_KEY` — Gemini
- `OPENAI_API_KEY` — GPT
- `AZURE_OPENAI_ENDPOINT` / `AZURE_OPENAI_DEPLOYMENT` — Azure OpenAI

## Architecture

### Modular Structure

```
Dendro/
├── app.R                    # Entry point: loads modules, assembles UI/server
├── R/
│   ├── mod_data_import.R    # File upload, demo data, data dictionary
│   ├── mod_data_quality.R   # Quality checks, outlier detection
│   ├── mod_factor_editor.R  # Factor level reordering
│   ├── mod_tree_config.R    # Model parameters (cp, minbucket, maxdepth)
│   ├── mod_tree_viz.R       # Static rpart.plot + interactive visNetwork
│   ├── mod_model_details.R  # Stats, importance, confusion matrix
│   ├── mod_ai_chat.R        # AI provider setup, personas, chat UI
│   ├── mod_export.R         # PowerPoint, HTML, PDF, image exports
│   ├── mod_guide.R          # Help/glossary
│   ├── utils_data.R         # Data processing (pure functions)
│   ├── utils_model.R        # Model building helpers (pure functions)
│   ├── utils_theme.R        # Theme definition and design tokens
│   └── utils_demo_data.R    # Demo dataset definitions
├── www/
│   └── styles.css
├── tests/
│   ├── testthat/
│   └── testthat.R
└── dendro.Rproj
```

### Conventions

- **Modules:** Each `mod_*.R` exports `*_ui(id)` and `*_server(id, ...)`. Modules communicate via returned reactive values, not global state.
- **Utils:** `utils_*.R` files are pure functions with no Shiny reactivity — independently testable.
- **`app.R`** is the orchestrator only: wires modules together, passes shared reactives.
- **Testing:** `testthat` for unit tests on utils, `shinytest2` for integration tests.

### Key Reactive State

- `rv$data` — uploaded/demo dataset
- `rv$model` — fitted rpart decision tree
- `rv$chat` — ellmer chat session
- `rv$chat_history` — conversation history
- `rv$data_dict` — variable labels and descriptions

### Key Dependencies

- `rpart` / `rpart.plot` — decision tree modeling and static visualization
- `visNetwork` — interactive tree visualization
- `ellmer` — multi-provider LLM integration (Azure, Claude, Gemini, GPT, Ollama)
- `bslib` — Bootstrap 5 theming
- `randomForest` — importance validation
- `officer` / `rvg` — PowerPoint export

## Design System — Modern Minimal

- **Aesthetic:** Clean, minimal, professional (Linear/Notion style)
- **Colors:** Single accent color, white backgrounds, near-black text, light gray borders
- **Typography:** Inter or system font stack
- **Components:** Flat cards with thin borders, solid buttons (no gradients), minimal shadows
- **No:** gradients, heavy shadows, bounce animations, decorative elements

## Production / Dev Mode

- **Production (`PRODUCTION_MODE = TRUE`):** Azure OpenAI only. Non-Azure providers hidden entirely.
- **Dev (`PRODUCTION_MODE = FALSE`):** All providers available, but non-Azure providers are **only enabled with demo datasets**. If the user uploads their own data, non-Azure providers are blocked with a warning. Auto-switch to Azure when switching from demo to uploaded data with a non-Azure provider selected.
- Flag set via environment variable or top-level constant in `app.R`.

## AI Persona Rules

- **Executive Summary & Project Team:** Interpretation only. No suggestions for additional analyses or parameter changes. Bullets and key takeaways, no long paragraphs.
- **Statistician:** May suggest next steps, parameter tuning, validation approaches. Full technical detail.
- **All personas:** Use data dictionary labels (not raw variable names). Lead with key takeaways as bullets. Bold important findings. Keep responses concise.

## Tree Visualization Rules

- **Classification:** Each class gets a distinct color from a qualitative palette (e.g., Set2). Colors must be clearly distinguishable.
- **Regression:** Sequential palette based on predicted value.
- **General:** Clean typography, rounded boxes, adequate spacing, white background, high-res for export.
