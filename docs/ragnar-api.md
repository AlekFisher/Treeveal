# ragnar API Reference (version: 0.3.0)
# Last updated: 2026-03-03
# Source: https://github.com/tidyverse/ragnar
# Docs: https://ragnar.tidyverse.org/

## Overview

Ragnar provides end-to-end RAG (Retrieval-Augmented Generation) workflows for R:
document loading → text chunking → embeddings → DuckDB vector store → retrieval → ellmer chat integration.

**Requires:** R >= 4.3.0, ellmer >= 0.3.0, duckdb >= 1.3.1, mirai >= 2.5.1, reticulate >= 1.42.0

---

## 1. Document Processing

### `read_as_markdown()` — Convert files to Markdown

```r
md <- read_as_markdown("path/to/file.pdf")       # PDF
md <- read_as_markdown("path/to/file.docx")       # Word
md <- read_as_markdown("https://example.com/page") # URL
md <- read_as_markdown("https://youtube.com/...", origin = "youtube")  # YouTube transcript
```

- Supports: PDF, DOCX, HTML, URLs, YouTube transcripts
- Uses MarkItDown (Python) under the hood via reticulate
- `origin` argument for document metadata
- HTML filtering: `html_extract_selectors` / `html_zap_selectors`

### `ragnar_find_links()` — Extract links from a page

```r
links <- ragnar_find_links("https://example.com/docs")
links <- ragnar_find_links("https://example.com/sitemap.xml")  # Sitemap support
links <- ragnar_find_links(url, children_only = TRUE)           # Only child links
links <- ragnar_find_links(url, validate = TRUE)                # Validate links exist
```

**BREAKING (v0.3.0):** `children_only` default changed from `TRUE` to `FALSE`. Now returns ALL links by default.

---

## 2. Text Chunking

### `markdown_chunk()` — Split markdown at semantic boundaries

```r
chunks <- markdown_chunk(md)                    # Default ~1600 chars
chunks <- markdown_chunk(md, chunk_size = 2000) # Custom size
```

- Splits at headings, paragraphs, and other semantic boundaries
- ~50% overlap between chunks by default
- Augments chunks with heading context (parent headings prepended)
- Returns `MarkdownDocumentChunks` S7 object

### `ragnar_chunks_view()` — Preview chunks interactively

```r
ragnar_chunks_view(chunks)  # Launches interactive viewer
```

---

## 3. Embedding Providers

| Function | Provider | Notes |
|---|---|---|
| `embed_openai(x, model)` | OpenAI | e.g., `"text-embedding-3-small"` |
| `embed_azure_openai()` | Azure AI Foundry | **New in 0.3.0** |
| `embed_ollama()` | Ollama (local) | Defaults to `embeddinggemma` model (v0.3.0) |
| `embed_bedrock()` | AWS Bedrock | |
| `embed_databricks()` | Databricks | v0.2.0+ |
| `embed_google_vertex()` | Google Vertex AI | v0.2.0+ |
| `embed_google_gemini()` | Google Gemini API | v0.2.1+ |
| `embed_lm_studio()` | LM Studio (local) | v0.2.1+ |
| `embed_snowflake()` | Snowflake Cortex | **New in 0.3.0** |

All embedding functions support configurable retry via:
```r
options(ragnar.embed.req_retry = list(max_tries = 5, backoff = ~ 2))
```

---

## 4. Store Management

### Create a store

```r
store <- ragnar_store_create(
  "knowledge_base.duckdb",
  embed = \(x) embed_openai(x, model = "text-embedding-3-small"),
  version = 2   # Default. v2 adds auto-deoverlap + heading augmentation
)

# MotherDuck (remote) support
store <- ragnar_store_create("md:my_database", embed = embed_fn)
```

### Connect to existing store

```r
store <- ragnar_store_connect("knowledge_base.duckdb")
```

### Insert, update, build index

```r
ragnar_store_insert(store, chunks)
ragnar_store_update(store, updated_chunks)
ragnar_store_build_index(store)   # Required after insertion for search
```

### Parallel ingestion (v0.3.0+)

```r
ragnar_store_ingest(store, paths)  # Uses mirai for parallel processing
```

### Inspect and visualize

```r
ragnar_store_inspect(store)  # Interactive Shiny inspector UI
ragnar_store_atlas(store)    # Embedding visualization (v0.3.0+)
```

---

## 5. Retrieval

### `ragnar_retrieve()` — Combined VSS + BM25

```r
results <- ragnar_retrieve(store, "my question", top_k = 10)

# Multi-query retrieval (v0.3.0+)
results <- ragnar_retrieve(store, c("question 1", "question 2"), top_k = 10)

# Filter results
results <- ragnar_retrieve(store, query, filter = ~ origin == "docs")
```

- Combines vector similarity search (VSS) and BM25 full-text search
- Deduplicates results automatically
- Returns data frame with text, metadata, and scores

### Individual search strategies

```r
# Vector similarity only
results <- ragnar_retrieve_vss(store, "query", top_k = 10)

# BM25 full-text only
results <- ragnar_retrieve_bm25(store, "query", top_k = 10,
  b = 0.75,          # Length normalization
  k = 1.2,           # Term frequency saturation
  conjunctive = FALSE # All terms must match if TRUE
)
```

### `chunks_deoverlap()` — Merge overlapping chunks

```r
clean_results <- chunks_deoverlap(results)
```

---

## 6. Chat Integration (with ellmer)

### Register retrieval as an LLM tool

```r
chat <- chat_openai()
ragnar_register_tool_retrieve(chat, store, top_k = 10)
chat$chat("What does the documentation say about X?")
```

- Tool auto-searches the store when the LLM decides it needs context
- Prevents re-retrieving already-seen chunks
- `name` and `title` parameters for custom tool naming

**BREAKING (v0.3.0):** Default tool name changed from `rag_retrieve_from_{name}` to `search_{name}`.

### Serve store over MCP (v0.3.0+)

```r
mcp_serve_store(store)  # For local MCP clients
```

---

## Complete Workflow Example

```r
library(ragnar)
library(ellmer)

# 1. Create store
store <- ragnar_store_create(
  "project_docs.duckdb",
  embed = \(x) embed_openai(x, model = "text-embedding-3-small")
)

# 2. Ingest documents
links <- ragnar_find_links("https://docs.example.com/", children_only = TRUE)
for (url in links) {
  chunks <- url |>
    read_as_markdown() |>
    markdown_chunk()
  ragnar_store_insert(store, chunks)
}
ragnar_store_build_index(store)

# 3a. Manual retrieval
results <- ragnar_retrieve(store, "authentication flow", top_k = 5)

# 3b. OR register as chat tool
chat <- chat_anthropic(system_prompt = "You are a helpful assistant.")
ragnar_register_tool_retrieve(chat, store, top_k = 10)
chat$chat("How does authentication work?")

# 4. Debug retrieval quality
ragnar_store_inspect(store)
ragnar_store_atlas(store)
```

---

## Known Gotchas

- **reticulate required:** `read_as_markdown()` depends on Python's MarkItDown. Ensure a Python environment is available.
- **Build index after insert:** Retrieval won't work until `ragnar_store_build_index()` is called.
- **Store version 2 is default:** v2 stores auto-deoverlap on retrieval and augment chunks with heading context. Old v1 stores may need recreation.
- **`tbl(store)` deprecated:** Use `tbl(store@con, "chunks")` instead.
- **Embedding dimension lock-in:** Once a store is created with an embedding model, you cannot change the embedding dimension. Create a new store if switching embedding models.
- **Tool name change in v0.3.0:** If system prompts or tool routing references `rag_retrieve_from_*`, update to `search_*`.

---

## Breaking Changes Log

| Version | Change | Migration |
|---|---|---|
| 0.3.0 | `ragnar_find_links()` `children_only` default: `TRUE` → `FALSE` | Add `children_only = TRUE` explicitly if needed |
| 0.3.0 | Tool name prefix: `rag_retrieve_from_` → `search_` | Update system prompts / tool references |
| 0.2.0 | `tbl(store)` deprecated | Use `tbl(store@con, "chunks")` |
| 0.2.0 | Store version 2 default (new schema) | Old v1 stores may need recreation |

## Version History

| Version | Date | Key Changes |
|---|---|---|
| 0.3.0 | 2026-01-23 | `embed_azure_openai()`, `embed_snowflake()`, `mcp_serve_store()`, multi-query retrieval, `ragnar_store_ingest()` parallel ingestion, `ragnar_store_atlas()`, tool name prefix change |
| 0.2.1 | 2025-08-19 | `embed_lm_studio()`, `embed_google_gemini()`, sitemap parsing, ellmer 0.3.0 compat, duckdb 1.3.1 compat |
| 0.2.0 | 2025-07-12 | Store v2, `markdown_chunk()`, `filter` in retrieval, MotherDuck support, `embed_google_vertex()`, `embed_databricks()`, `ragnar_chunks_view()` |
| 0.1.0 | 2025-05-30 | Initial CRAN release |
