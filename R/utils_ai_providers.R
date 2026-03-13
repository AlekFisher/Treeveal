# utils_ai_providers.R
# Shared provider helpers for Azure-backed and dev-only AI services.

trim_env_var <- function(name) {
  trimws(Sys.getenv(name, unset = ""))
}

build_single_model_choice <- function(model_value, label_prefix, missing_var) {
  model_value <- trimws(if (is.null(model_value)) "" else model_value)

  if (!nzchar(model_value)) {
    return(setNames("", paste0("Set ", missing_var)))
  }

  setNames(model_value, paste0(label_prefix, ": ", model_value))
}

get_azure_provider_configs <- function() {
  list(
    azure = list(
      label = "Azure OpenAI (Primary)",
      endpoint_var = "AZURE_OPENAI_ENDPOINT",
      api_key_var = "AZURE_OPENAI_API_KEY",
      deployment_var = "AZURE_OPENAI_DEPLOYMENT",
      models = build_single_model_choice(
        trim_env_var("AZURE_OPENAI_DEPLOYMENT"),
        label_prefix = "Primary",
        missing_var = "AZURE_OPENAI_DEPLOYMENT"
      )
    ),
    azure_new = list(
      label = "Azure OpenAI (Enterprise Claude)",
      endpoint_var = "AZURE_OPENAI_ENDPOINT_NEW",
      api_key_var = "AZURE_OPENAI_API_KEY_NEW",
      deployment_var = "AZURE_OPENAI_DEPLOYMENT_NEW",
      models = build_single_model_choice(
        trim_env_var("AZURE_OPENAI_DEPLOYMENT_NEW"),
        label_prefix = "Enterprise Claude",
        missing_var = "AZURE_OPENAI_DEPLOYMENT_NEW"
      )
    )
  )
}

is_configured_azure_provider <- function(provider_id) {
  configs <- get_azure_provider_configs()
  provider <- configs[[provider_id]]

  if (is.null(provider)) {
    return(FALSE)
  }

  required_vars <- c(
    provider$api_key_var,
    provider$endpoint_var,
    provider$deployment_var
  )

  all(vapply(required_vars, function(env_var) {
    nzchar(trim_env_var(env_var))
  }, logical(1)))
}

get_visible_azure_provider_ids <- function() {
  provider_ids <- names(get_azure_provider_configs())
  configured_ids <- provider_ids[vapply(provider_ids, is_configured_azure_provider, logical(1))]

  if (length(configured_ids) > 0) {
    configured_ids
  } else {
    "azure"
  }
}

get_default_secure_provider_id <- function() {
  visible_ids <- get_visible_azure_provider_ids()

  if ("azure" %in% visible_ids) {
    "azure"
  } else {
    visible_ids[[1]]
  }
}

get_secure_provider_choices <- function() {
  configs <- get_azure_provider_configs()
  provider_ids <- get_visible_azure_provider_ids()
  labels <- vapply(provider_ids, function(provider_id) {
    configs[[provider_id]]$label
  }, character(1))

  setNames(provider_ids, labels)
}

get_ai_provider_label <- function(provider_id) {
  azure_configs <- get_azure_provider_configs()

  if (provider_id %in% names(azure_configs)) {
    return(azure_configs[[provider_id]]$label)
  }

  switch(
    provider_id,
    ollama = "Local (Ollama)",
    anthropic = "Anthropic (Claude)",
    gemini = "Google (Gemini)",
    openai = "OpenAI (GPT)",
    provider_id
  )
}

get_ai_model_choices <- function(provider_id) {
  azure_configs <- get_azure_provider_configs()

  if (provider_id %in% names(azure_configs)) {
    return(azure_configs[[provider_id]]$models)
  }

  switch(
    provider_id,
    ollama = c("Ministral 3 (8B)" = "ministral-3:8b"),
    anthropic = c(
      "claude-haiku-4-5-20251001" = "claude-haiku-4-5-20251001",
      "claude-sonnet-4-20250514" = "claude-sonnet-4-20250514",
      "claude-opus-4-20250514" = "claude-opus-4-20250514"
    ),
    gemini = c("gemini-3-flash-preview" = "gemini-3-flash-preview"),
    openai = c(
      "gpt-5-mini" = "gpt-5-mini",
      "gpt-4o" = "gpt-4o",
      "gpt-4o-mini" = "gpt-4o-mini"
    ),
    c("gpt-5-mini" = "gpt-5-mini")
  )
}

get_default_ai_model <- function(provider_id) {
  choices <- get_ai_model_choices(provider_id)

  if (length(choices) == 0) {
    return(NULL)
  }

  unname(choices[[1]])
}

is_secure_ai_provider <- function(provider_id) {
  provider_id %in% names(get_azure_provider_configs())
}
