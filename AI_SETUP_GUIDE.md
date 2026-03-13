# AI Setup Guide for Dendro

Canonical reference for Dendro's `ellmer` setup. This replaces the older split between `AI_SETUP_GUIDE.md` and `docs/ELLMER_MODEL_SETTINGS.md`.

## 1. Environment Variables

Set these in `.Renviron` for local work, or in Posit Connect app settings for deployment.

```env
# Secure Azure provider: primary instance
AZURE_OPENAI_API_KEY=""
AZURE_OPENAI_ENDPOINT="https://your-primary-resource.openai.azure.com/"
AZURE_OPENAI_DEPLOYMENT="gpt-5-mini"

# Secure Azure provider: secondary instance / enterprise Claude
AZURE_OPENAI_API_KEY_NEW=""
AZURE_OPENAI_ENDPOINT_NEW="https://your-secondary-resource.openai.azure.com/"
AZURE_OPENAI_DEPLOYMENT_NEW="enterprise-claude"

# Dev-only external providers
ANTHROPIC_API_KEY=""
GOOGLE_API_KEY=""
OPENAI_API_KEY=""

# App mode
PRODUCTION_MODE="FALSE"
```

Rules:
- `AZURE_OPENAI_DEPLOYMENT` and `AZURE_OPENAI_DEPLOYMENT_NEW` must be Azure deployment names, not public model IDs unless you named them that way in Azure.
- The secondary Azure instance is treated as secure because traffic still stays on enterprise Azure infrastructure.
- `.Renviron` must end with a newline or R may ignore the last entry.
- Never commit secrets. Keep `.Renviron` in `.gitignore`.

## 2. Provider Behavior in Dendro

`app.R` reads the mode flag once:

```r
PRODUCTION_MODE <- as.logical(Sys.getenv("PRODUCTION_MODE", "TRUE"))
```

Production mode:
- Only Azure-backed providers are shown.
- If both Azure instances are configured, both appear in the provider dropdown.
- This allows standard Azure OpenAI and the enterprise Claude deployment to coexist without exposing public providers.

Dev mode:
- The same Azure-backed providers are available.
- Ollama, Anthropic, Gemini, and OpenAI are also visible.
- Non-Azure providers are blocked for uploaded data and allowed only with demo datasets.
- If a user switches from demo data to uploaded data while on a non-Azure provider, the app auto-switches to the default secure Azure provider.

## 3. Shared Azure Provider Registry

The app now uses one helper file, [R/utils_ai_providers.R](C:/Adelphi Apps/Dendro/R/utils_ai_providers.R), to keep the UI and chat initialization in sync.

Key helpers:
- `get_azure_provider_configs()`
- `get_secure_provider_choices()`
- `get_ai_model_choices()`
- `is_secure_ai_provider()`

Current registry shape:

```r
get_azure_provider_configs <- function() {
  list(
    azure = list(
      label = "Azure OpenAI (Primary)",
      endpoint_var = "AZURE_OPENAI_ENDPOINT",
      api_key_var = "AZURE_OPENAI_API_KEY",
      deployment_var = "AZURE_OPENAI_DEPLOYMENT"
    ),
    azure_new = list(
      label = "Azure OpenAI (Enterprise Claude)",
      endpoint_var = "AZURE_OPENAI_ENDPOINT_NEW",
      api_key_var = "AZURE_OPENAI_API_KEY_NEW",
      deployment_var = "AZURE_OPENAI_DEPLOYMENT_NEW"
    )
  )
}
```

Why this matters:
- the provider dropdown and model dropdown come from the same source
- both secure Azure instances pass the uploaded-data guard
- adding another Azure instance later is a small config change instead of another branch of copied logic

## 4. Chat Initialization Pattern

For Azure-backed providers, Dendro resolves the selected provider first, then creates the chat with that provider's endpoint, key, and deployment:

```r
azure_config <- get_azure_provider_configs()[[input$ai_provider]]

rv$chat <- ellmer::chat_azure_openai(
  endpoint = trimws(Sys.getenv(azure_config$endpoint_var)),
  model = input$ai_model,
  credentials = function() trimws(Sys.getenv(azure_config$api_key_var)),
  api_version = "2025-04-01-preview",
  system_prompt = system_prompt,
  echo = "none"
)
```

Notes:
- Dendro uses `credentials = function() ...` instead of the deprecated `api_key =`.
- `echo = "none"` is the current `ellmer` setting used in the app.
- The secondary Azure provider works exactly like the primary one from `ellmer`'s point of view; only the endpoint, key, and deployment name change.

## 5. Shiny Lifecycle Rules

The chat object is recreated whenever any of these change:
- provider
- model
- persona
- clear chat action

That is important because the system prompt and the active backend can both change across those events.

## 6. Uploaded-Data Security Rule

For uploaded data, only secure Azure providers are allowed:

```r
if (!production_mode && !isTRUE(rv$is_demo_data) &&
    !is_secure_ai_provider(input$ai_provider)) {
  # block request and tell the user to switch to Azure
}
```

This rule now treats both `azure` and `azure_new` as compliant.

## 7. Posit Connect Checklist

1. Set `PRODUCTION_MODE="TRUE"`.
2. Set the primary Azure variables if the primary instance should be available.
3. Set the `*_NEW` Azure variables if the enterprise Claude instance should be available.
4. Do not set public-provider keys in production unless you intentionally want them exposed in a non-production deployment.
5. Confirm the deployed app can send a test prompt through each Azure provider you expect to expose.

## 8. Minimal Dendro-Specific Setup

For a deployment that supports both secure Azure paths:

```env
PRODUCTION_MODE="TRUE"
AZURE_OPENAI_API_KEY="..."
AZURE_OPENAI_ENDPOINT="https://primary.openai.azure.com/"
AZURE_OPENAI_DEPLOYMENT="gpt-5-mini"
AZURE_OPENAI_API_KEY_NEW="..."
AZURE_OPENAI_ENDPOINT_NEW="https://secondary.openai.azure.com/"
AZURE_OPENAI_DEPLOYMENT_NEW="enterprise-claude"
```

If the secondary instance is the only secure option you want to expose, set only the `*_NEW` variables and the app will use that Azure provider as the secure default.
