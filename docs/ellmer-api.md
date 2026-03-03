# ellmer API Reference (version: 0.4.0)
# Last updated: 2026-02-25
# Source: https://github.com/tidyverse/ellmer

## Creating a Chat Object

### `chat_anthropic()` / `chat_claude()` (aliases)

```r
chat <- chat_anthropic(
  system_prompt = NULL,
  params = NULL,
  model = NULL,                    # Default: "claude-sonnet-4-5-20250929"
  cache = c("5m", "1h", "none"),   # Default: "5m"
  api_args = list(),
  base_url = "https://api.anthropic.com/v1",
  beta_headers = character(),
  api_key = NULL,                  # Deprecated; use credentials
  credentials = NULL,              # Zero-arg function returning API key string
  api_headers = character(),
  echo = NULL                      # "none" | "output" | "all"
)
```

- Set `ANTHROPIC_API_KEY` env var (best in `.Renviron` via `usethis::edit_r_environ()`)
- `echo = "output"` is the default at console; `echo = "none"` in functions
- Caching is enabled by default ("5m"). Caching requires 1024-4096 tokens minimum.

### Generic `chat()` helper

```r
# Pick provider/model via string
chat <- chat("anthropic/claude-sonnet-4-5-20250929", system_prompt = "You are helpful.")
```

### `params()` — Common model parameters

```r
chat <- chat_anthropic(
  params = params(
    temperature = 0.7,
    max_tokens = 4096,
    top_p = NULL,
    top_k = NULL,
    seed = NULL,
    stop_sequences = NULL,
    reasoning_effort = NULL,   # "low", "medium", "high"
    reasoning_tokens = NULL    # integer token budget
  )
)
```

## Registering Tools

### `tool()` — Define a tool (v0.3.0+ syntax)

```r
my_tool <- tool(
  fun,                        # The R function to call
  description,                # Detailed description for the LLM
  ...,                        # Deprecated — use `arguments` instead
  arguments = list(),         # Named list of type_*() definitions
  name = NULL,                # Auto-inferred if `fun` is a named function
  convert = TRUE,             # Auto-convert JSON to R types
  annotations = list()        # Optional MCP annotations via tool_annotations()
)
```

### Complete working example

```r
search_variables <- function(query, top_n = 5L) {
  # ... search logic ...
  paste("Found", top_n, "variables matching", query)
}

tool_search <- tool(
  search_variables,
  description = "Search the variable registry for variables matching a query string. Returns variable names, labels, and types.",
  arguments = list(
    query = type_string("The search term to match against variable names and labels."),
    top_n = type_integer("Maximum number of results to return. Defaults to 5.")
  )
)

chat <- chat_anthropic(system_prompt = "You are a data analyst.")
chat$register_tool(tool_search)
chat$chat("Find variables about satisfaction")
```

### Register multiple tools at once

```r
chat$register_tools(list(tool_search, tool_frequency, tool_crosstab))
```

### Type specification functions

| Function | R Equivalent | Example |
|---|---|---|
| `type_string(desc)` | length-1 character | `type_string("Variable name")` |
| `type_integer(desc)` | length-1 integer | `type_integer("Number of results")` |
| `type_number(desc)` | length-1 double | `type_number("Significance level")` |
| `type_boolean(desc)` | length-1 logical | `type_boolean("Include weights?")` |
| `type_enum(values, desc)` | length-1 factor | `type_enum(c("90", "95", "99"), "Confidence level")` |
| `type_array(items, desc)` | vector/list | `type_array(type_string(), "List of var names")` |
| `type_object(..., .description)` | named list | See below |
| `type_ignore()` | (skip this arg) | For args with defaults the LLM shouldn't set |

All type functions accept `required = TRUE/FALSE` (default TRUE). Set `required = FALSE` for optional params (must have a default in the R function).

### `type_object()` example

```r
type_object(
  .description = "Analysis parameters",
  row_var = type_string("The row variable name"),
  col_var = type_string("The column/banner variable name"),
  weight_var = type_string("Weight variable name", required = FALSE),
  sig_level = type_number("Significance level (0.90, 0.95, or 0.99)", required = FALSE)
)
```

## Chat Methods — Sending Messages

### `$chat()` — Synchronous, returns string

```r
response <- chat$chat("What variables relate to satisfaction?")
# Returns a character string (likely Markdown)
```

### `$stream()` — Synchronous streaming (coro generator)

```r
stream <- chat$stream("Analyze this data")
# Returns a coro generator that yields strings
coro::loop(for (chunk in stream) {
  cat(chunk)
})
```

### `$chat_async()` — Async, returns promise

```r
promise <- chat$chat_async("Analyze this", tool_mode = "sequential")
# tool_mode: "concurrent" (default) or "sequential" (better for interactive UIs)
```

### `$stream_async()` — Async streaming (coro async generator)

```r
stream <- chat$stream_async(
  "Analyze this",
  tool_mode = "sequential",   # "concurrent" | "sequential"
  stream = "text"             # "text" | "content"
)
```

**For Shiny apps, use `$chat_async()` or `$stream_async()` with `tool_mode = "sequential"` for interactive tool approval.**

## Conversation History Management

### Get turns

```r
turns <- chat$get_turns()                          # Without system prompt
turns <- chat$get_turns(include_system_prompt = TRUE)  # With system prompt
```

### Set/replace turns

```r
chat$set_turns(list())   # Clear all history
chat$set_turns(saved_turns)  # Restore saved conversation
```

### Add a turn pair

```r
chat$add_turn(user_turn, assistant_turn)
```

### Get/set system prompt

```r
chat$get_system_prompt()
chat$set_system_prompt("New system prompt")
```

### Last turn

```r
chat$last_turn()                     # Last assistant turn
chat$last_turn(role = "user")        # Last user turn
```

### Reset conversation (keep tools, change prompt)

```r
# Create a fresh chat — simplest way to "clear chat"
chat <- chat_anthropic(
  system_prompt = updated_prompt,
  model = "claude-sonnet-4-5-20250929"
)
chat$register_tools(my_tools)
```

## Token Usage & Cost

```r
chat$get_tokens()   # Data frame: input, output, cached_input, cost (per turn)
chat$get_cost()     # Total cumulative cost
chat$get_cost(include = "last")  # Cost of last turn only
token_usage()       # Session-wide summary across all chats
```

## Tool Call/Response Flow

1. User sends message via `$chat()` or `$stream()`
2. LLM may return a tool request (ContentToolRequest)
3. ellmer automatically calls the R function with provided arguments
4. Result is sent back to the LLM as ContentToolResult
5. LLM may make additional tool calls or return final text
6. The tool loop is handled automatically — you just call `$chat()`

### Intercepting tool calls

```r
# Before tool executes:
chat$on_tool_request(function(request) {
  cat("Tool requested:", request@name, "\n")
  # Call tool_reject() to prevent execution
})

# After tool executes:
chat$on_tool_result(function(result) {
  cat("Tool result received\n")
})
```

### Rejecting tool calls

```r
# Inside a tool function:
my_tool_fn <- function(var_name) {
  if (!var_name %in% valid_names) {
    tool_reject("Variable not found in registry")
  }
  # ... normal logic ...
}

# Or in on_tool_request callback:
chat$on_tool_request(function(request) {
  if (!user_approved) tool_reject("User declined")
})
```

## Content Formatting

```r
contents_text(turn)      # Plain text only
contents_markdown(turn)  # Markdown (text + images)
contents_markdown(chat)  # Entire chat history as markdown
contents_html(turn)      # HTML (requires commonmark)
```

## Prompt Interpolation

```r
# Inline
prompt <- interpolate(
  "You have access to {{n_vars}} variables. Summary: {{summary}}",
  n_vars = 150,
  summary = registry_summary
)

# From file
prompt <- interpolate_file("prompts/system.md", n_vars = 150)
```

Use `{{ }}` (double braces) for interpolation — this avoids conflicts with R code and JSON in prompts.

## Helper: `create_tool_def()`

Interactive helper that uses an LLM to generate `tool()` calls from documented functions:

```r
create_tool_def(my_function)          # Uses function's roxygen docs
create_tool_def(stats::cor)           # Works with package functions
create_tool_def("my_function")        # String form
```

Not for production code — use interactively to bootstrap tool definitions.

## Known Gotchas for v0.4.0

- **Tool argument descriptions matter.** The more detail in `type_*()` descriptions, the better the LLM selects arguments. Vague descriptions lead to wrong tool calls.
- **Tool functions should return character strings** for best results. Complex R objects may not serialize well for the LLM to interpret.
- **`tool()` syntax changed in v0.3.0.** Arguments are now in a named `arguments` list, not `...`. Old code using `tool(fn, "desc", x = type_string())` must be updated to `tool(fn, description = "desc", arguments = list(x = type_string()))`.
- **Caching is on by default** for Anthropic. First turn is more expensive (1.25x write cost), subsequent turns save on cached tokens (0.1x).
- **`$chat_async()` with `tool_mode = "sequential"`** is recommended for Shiny apps where tools may need user interaction.
- **System prompt changes** don't clear history. Create a new Chat object to fully reset.
- **`type_object()` uses `.description` (dot prefix)** unlike other type functions which use `description`.
