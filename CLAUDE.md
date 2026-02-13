# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Repository Overview

This is a collection of R projects. Currently contains one project:

### Dendro

An R Shiny application for interactive decision tree analysis with AI-powered interpretation. Located in `Dendro/`.

## Running the Application

```r
# From the Dendro directory
shiny::runApp()

# Or open Dendro.Rproj in RStudio and click "Run App"
```

## Required R Packages

```r
install.packages(c("shiny", "bslib", "rpart", "rpart.plot", "ellmer", "DT", "dplyr", "tidyr", "ggplot2"))
```

## Environment Configuration

API keys for AI providers should be set in `.Renviron`:
- `ANTHROPIC_API_KEY` - Claude
- `GOOGLE_API_KEY` - Gemini
- `OPENAI_API_KEY` - GPT

## Architecture (Dendro)

**Single-file Shiny app** (`app.R`) using reactive programming:

- `rv$data` - uploaded/demo dataset
- `rv$model` - fitted rpart decision tree
- `rv$chat` - ellmer chat session for AI interpretation
- `rv$chat_history` - conversation history

**Key dependencies:**
- `rpart` for decision tree modeling
- `ellmer` for multi-provider LLM integration (Claude, Gemini, GPT, Ollama)
- `bslib` for Bootstrap 5 theming

**UI structure:** Sidebar with configuration panels, main area with tabs for Data Preview, Decision Tree visualization, AI Interpretation chat, and Model Details.
