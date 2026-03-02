# AI Setup Guide for R Shiny Apps with ellmer

Portable reference for adding multi-provider LLM chat to any R Shiny application.
Copy this file into new projects and adapt as needed.

---

## 1. Environment Variables (.Renviron)

Create a `.Renviron` file in your project root. Shiny loads it automatically on startup.

```env
# === Dev providers (personal API keys) ===
ANTHROPIC_API_KEY = "sk-ant-api03-..."
GOOGLE_API_KEY = "AIza..."
OPENAI_API_KEY = "sk-proj-..."

# === Local testing ===
# No key needed — Ollama runs at http://localhost:11434

# === Production (Azure OpenAI) ===
AZURE_OPENAI_API_KEY = ""
AZURE_OPENAI_ENDPOINT = "https://<your-resource>.openai.azure.com/"
AZURE_OPENAI_DEPLOYMENT = "gpt-4o"

# === Mode toggle ===
PRODUCTION_MODE = "FALSE"
```

**Key rules:**
- `.Renviron` must end with a newline or R silently ignores the last line.
- Add `.Renviron` to `.gitignore` — never commit keys.
- On Posit Connect, set these as environment variables in the app settings instead.
- Each developer maintains their own `.Renviron` locally; share a `.Renviron.example` (with blank values) in the repo.

### .Renviron.example (commit this)

```env
# Copy to .Renviron and fill in your keys
ANTHROPIC_API_KEY = ""
GOOGLE_API_KEY = ""
OPENAI_API_KEY = ""
AZURE_OPENAI_API_KEY = ""
AZURE_OPENAI_ENDPOINT = ""
AZURE_OPENAI_DEPLOYMENT = ""
PRODUCTION_MODE = "FALSE"
```

---

## 2. Production / Dev Mode Pattern

Read the flag once at the top of `app.R`:

```r
PRODUCTION_MODE <- as.logical(Sys.getenv("PRODUCTION_MODE", "TRUE"))
```

**Production (`TRUE`):**
- Azure OpenAI only. All other providers hidden from the UI.
- Safe for client/sensitive data — traffic stays within your Azure tenant.
- Default is `TRUE` so deployed apps are secure even if someone forgets to set it.

**Dev (`FALSE`):**
- All providers visible: Azure, Ollama, Claude, Gemini, GPT.
- Non-Azure providers are **only enabled with demo/sample data**.
- If the user uploads real data, auto-switch to Azure and warn.
- This prevents accidentally sending client data to third-party APIs during development.

### UI pattern

```r
# In your sidebar/config UI
if (production_mode) {
  selectInput("ai_provider", "AI Provider",
    choices = c("Azure OpenAI (Secure)" = "azure"),
    selected = "azure"
  )
} else {
  selectInput("ai_provider", "AI Provider",
    choices = c(
      "Azure OpenAI (Secure)" = "azure",
      "Local (Ollama)"        = "ollama",
      "Anthropic (Claude)"    = "anthropic",
      "Google (Gemini)"       = "gemini",
      "OpenAI (GPT)"          = "openai"
    ),
    selected = "openai"
  )
}
```

### Auto-switch guard (server)

```r
# When data source changes, force Azure if non-demo data
observeEvent(rv$is_demo_data, {
  if (!production_mode && !isTRUE(rv$is_demo_data) &&
      !is.null(input$ai_provider) && input$ai_provider != "azure") {
    updateSelectInput(session, "ai_provider", selected = "azure")
    showNotification(
      "Switched to Azure OpenAI — non-Azure providers are only available with demo datasets.",
      type = "warning", duration = 6
    )
  }
})
```

---

## 3. ellmer Provider Setup (Copy-Paste Recipes)

The `ellmer` package provides a unified chat interface across providers.
Each `chat_*()` function returns an object with a `$chat(message)` method.

### Azure OpenAI (production)

```r
chat <- ellmer::chat_azure_openai(
  endpoint     = Sys.getenv("AZURE_OPENAI_ENDPOINT"),
  model        = Sys.getenv("AZURE_OPENAI_DEPLOYMENT"),
  api_key      = Sys.getenv("AZURE_OPENAI_API_KEY"),
  api_version  = "2025-04-01-preview",
  system_prompt = system_prompt,
  echo         = FALSE
)
```

- `api_version`: check Azure docs for the latest stable version.
- The deployment name IS the model name in Azure.
- `echo = FALSE` suppresses console output in production.

### Ollama (local testing — free, no key)

```r
chat <- ellmer::chat_ollama(
  model         = "ministral-3:8b",
  system_prompt = system_prompt
)
```

- Requires Ollama running locally (`ollama serve`).
- Pull models first: `ollama pull ministral-3:8b`.
- Great for offline development and prompt iteration without API costs.
- Smaller models are fast but less capable — good enough for testing the plumbing.

### Anthropic (Claude)

```r
chat <- ellmer::chat_anthropic(
  model         = "claude-haiku-4-5-20251001",
  system_prompt = system_prompt
)
```

- `ellmer` reads `ANTHROPIC_API_KEY` from the environment automatically.
- Model options: `claude-haiku-4-5-20251001` (fast/cheap), `claude-sonnet-4-20250514` (balanced), `claude-opus-4-20250514` (most capable).

### Google Gemini

```r
chat <- ellmer::chat_google_gemini(
  model         = "gemini-3-flash-preview",
  system_prompt = system_prompt,
  api_key       = Sys.getenv("GOOGLE_API_KEY")
)
```

- Get a key at https://aistudio.google.com/apikey.
- Flash models are fast and cheap for dev work.

### OpenAI (GPT)

```r
chat <- ellmer::chat_openai(
  model         = "gpt-4o-mini",
  system_prompt = system_prompt
)
```

- `ellmer` reads `OPENAI_API_KEY` from the environment automatically.
- Model options: `gpt-4o-mini` (fast/cheap), `gpt-4o` (capable), `gpt-5-mini` (latest).

---

## 4. Chat Object Lifecycle in Shiny

Store the chat object in shared `reactiveValues` so it persists across messages:

```r
rv <- reactiveValues(
  chat         = NULL,    # ellmer chat object (holds conversation state)
  chat_history = list()   # list of list(role, content) for UI display
)
```

### Initialization pattern

Create (or reuse) the chat object lazily — only when the user first sends a message:

```r
get_chat <- function() {
  if (is.null(rv$chat)) {
    # Build system_prompt from current app state...
    # Create chat object based on selected provider...
    rv$chat <- ellmer::chat_openai(model = input$ai_model, system_prompt = system_prompt)
  }
  rv$chat
}
```

### Reset the chat when context changes

```r
# Provider changed → new chat object needed
observeEvent(input$ai_provider, { rv$chat <- NULL })

# Persona/mode changed → new system prompt needed
observeEvent(input$persona, { rv$chat <- NULL })

# Clear button
observeEvent(input$clear_chat, {
  rv$chat_history <- list()
  rv$chat <- NULL
})
```

---

## 5. Sending Messages and Handling Errors

```r
send_to_ai <- function(message) {
  # Append user message to display history
  rv$chat_history <- c(rv$chat_history, list(
    list(role = "user", content = message)
  ))

  # Get or create chat object
  chat <- tryCatch(get_chat(), error = function(e) {
    rv$chat_history <- c(rv$chat_history, list(
      list(role = "assistant", content = paste0("**Configuration Error**\n\n", e$message))
    ))
    return(NULL)
  })
  if (is.null(chat)) return(invisible(NULL))

  # Send and capture response
  tryCatch({
    response <- chat$chat(message)
    rv$chat_history <- c(rv$chat_history, list(
      list(role = "assistant", content = response)
    ))
  }, error = function(e) {
    # Classify error for user-friendly message
    msg <- e$message
    user_msg <- if (grepl("401|unauthorized|invalid.*key", msg, ignore.case = TRUE)) {
      "**Auth Failed** — check your API key."
    } else if (grepl("429|rate.?limit", msg, ignore.case = TRUE)) {
      "**Rate Limited** — wait a moment and retry."
    } else if (grepl("timeout|connection", msg, ignore.case = TRUE)) {
      "**Connection Error** — check your network."
    } else {
      paste0("**Error:** ", msg)
    }
    rv$chat_history <- c(rv$chat_history, list(
      list(role = "assistant", content = user_msg)
    ))
  })
}
```

Wire it up with a progress indicator:

```r
observeEvent(input$send_message, {
  req(input$user_message, nzchar(trimws(input$user_message)))
  msg <- trimws(input$user_message)
  updateTextAreaInput(session, "user_message", value = "")
  withProgress(message = "Thinking...", { send_to_ai(msg) })
})
```

---

## 6. System Prompt Construction

Build the system prompt dynamically from current app state. This is the key part that changes per project — everything else above is boilerplate.

```r
system_prompt <- paste0(
  "You are an expert analyst helping a user interpret their results.\n\n",

  # Dynamic context from app state (model summary, data stats, etc.)
  get_context_string(),

  # Optional user-provided context
  if (nzchar(input$study_context)) {
    paste0("\n\nUser Context:\n", input$study_context)
  } else "",

  # Formatting rules (keep consistent across projects)
  "\n\nFORMATTING RULES:\n",
  "- Lead with key takeaways as bullet points\n",
  "- Use markdown headers and lists\n",
  "- **Bold** important findings\n",
  "- Keep responses concise\n",

  # Persona-specific instructions
  persona_instructions
)
```

---

## 7. Model Choices Quick Reference

Use this table to populate your model dropdowns. Update as new models release.

| Provider  | Model ID                       | Speed  | Cost   | Best for             |
|-----------|--------------------------------|--------|--------|----------------------|
| Azure     | (your deployment name)         | varies | varies | Production           |
| Ollama    | ministral-3:8b                 | Fast   | Free   | Local dev/testing    |
| Claude    | claude-haiku-4-5-20251001      | Fast   | Low    | Dev, quick tasks     |
| Claude    | claude-sonnet-4-20250514       | Med    | Med    | Balanced dev         |
| GPT       | gpt-4o-mini                    | Fast   | Low    | Dev, quick tasks     |
| GPT       | gpt-4o                         | Med    | Med    | Quality dev          |
| Gemini    | gemini-3-flash-preview         | Fast   | Low    | Dev, quick tasks     |

---

## 8. Posit Connect Deployment Checklist

1. Set `PRODUCTION_MODE = "TRUE"` in the app's environment variables on Connect.
2. Set `AZURE_OPENAI_API_KEY`, `AZURE_OPENAI_ENDPOINT`, `AZURE_OPENAI_DEPLOYMENT`.
3. Do NOT set `ANTHROPIC_API_KEY`, `GOOGLE_API_KEY`, or `OPENAI_API_KEY` on Connect — they should not exist in production.
4. Confirm the `ellmer` package is available in your Connect environment (add to `renv.lock` or install on the server).
5. Test the deployed app with the Azure provider before sharing.

---

## 9. Keeping Secrets Safe

### .gitignore (keep keys out of version control)

```gitignore
.Renviron
.env
```

### Claude Code deny rules (keep keys out of AI sessions)

Add this to `~/.claude/settings.json` so Claude Code can never read your key files:

```json
{
  "permissions": {
    "deny": [
      "Read(**/.Renviron)",
      "Read(**/.env)"
    ]
  }
}
```

This is a global setting — once set, it applies to all projects.

When Claude Code needs to know which keys you have configured, just tell it the variable names (e.g. "I have `ANTHROPIC_API_KEY` and `GOOGLE_API_KEY` set"). It never needs the actual values — only the names to write `Sys.getenv()` calls.

### Required environment variable names

| Variable                    | Provider        | Where to get it                              |
|-----------------------------|-----------------|----------------------------------------------|
| `ANTHROPIC_API_KEY`         | Claude          | https://console.anthropic.com/               |
| `GOOGLE_API_KEY`            | Gemini          | https://aistudio.google.com/apikey           |
| `OPENAI_API_KEY`            | GPT             | https://platform.openai.com/api-keys         |
| `AZURE_OPENAI_API_KEY`      | Azure OpenAI    | Azure portal (your org admin)                |
| `AZURE_OPENAI_ENDPOINT`     | Azure OpenAI    | Azure portal (e.g. `https://xxx.openai.azure.com/`) |
| `AZURE_OPENAI_DEPLOYMENT`   | Azure OpenAI    | Azure portal (deployment name = model name)  |
| `PRODUCTION_MODE`           | App config      | `"TRUE"` or `"FALSE"`                        |
| *(none needed)*             | Ollama (local)  | Just install Ollama and `ollama pull <model>` |

---

## 10. Minimal Working Example

A stripped-down app showing the full pattern in ~60 lines:

```r
library(shiny)
library(bslib)
library(ellmer)

PRODUCTION_MODE <- as.logical(Sys.getenv("PRODUCTION_MODE", "TRUE"))

ui <- page_sidebar(
  title = "My App",
  sidebar = sidebar(
    selectInput("ai_provider", "Provider",
      choices = if (PRODUCTION_MODE) {
        c("Azure OpenAI" = "azure")
      } else {
        c("Azure OpenAI" = "azure", "Ollama" = "ollama",
          "Claude" = "anthropic", "Gemini" = "gemini", "GPT" = "openai")
      }
    ),
    textAreaInput("user_message", "Ask something:", rows = 3),
    actionButton("send", "Send", class = "btn-primary w-100")
  ),
  card(
    card_header("AI Response"),
    card_body(uiOutput("response"))
  )
)

server <- function(input, output, session) {
  rv <- reactiveValues(chat = NULL, last_response = "")

  get_chat <- function() {
    if (is.null(rv$chat)) {
      sp <- "You are a helpful assistant."
      rv$chat <- switch(input$ai_provider,
        "azure"     = chat_azure_openai(
                        endpoint = Sys.getenv("AZURE_OPENAI_ENDPOINT"),
                        model = Sys.getenv("AZURE_OPENAI_DEPLOYMENT"),
                        api_key = Sys.getenv("AZURE_OPENAI_API_KEY"),
                        api_version = "2025-04-01-preview",
                        system_prompt = sp, echo = FALSE),
        "ollama"    = chat_ollama(model = "ministral-3:8b", system_prompt = sp),
        "anthropic" = chat_anthropic(model = "claude-haiku-4-5-20251001", system_prompt = sp),
        "gemini"    = chat_google_gemini(model = "gemini-3-flash-preview",
                        system_prompt = sp, api_key = Sys.getenv("GOOGLE_API_KEY")),
        "openai"    = chat_openai(model = "gpt-4o-mini", system_prompt = sp)
      )
    }
    rv$chat
  }

  observeEvent(input$ai_provider, { rv$chat <- NULL })

  observeEvent(input$send, {
    req(input$user_message, nzchar(trimws(input$user_message)))
    withProgress(message = "Thinking...", {
      tryCatch({
        chat <- get_chat()
        rv$last_response <- chat$chat(trimws(input$user_message))
      }, error = function(e) {
        rv$last_response <- paste("Error:", e$message)
      })
    })
    updateTextAreaInput(session, "user_message", value = "")
  })

  output$response <- renderUI({
    if (nzchar(rv$last_response)) {
      HTML(markdown::markdownToHTML(text = rv$last_response, fragment.only = TRUE))
    } else {
      p(class = "text-muted", "Send a message to get started.")
    }
  })
}

shinyApp(ui, server)
```
