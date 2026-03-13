# test-utils_ai_providers.R

with_env_overrides <- function(new_values, code) {
  old_values <- Sys.getenv(names(new_values), unset = NA_character_)

  on.exit({
    restore_values <- old_values[!is.na(old_values)]
    unset_names <- names(old_values)[is.na(old_values)]

    if (length(restore_values) > 0) {
      do.call(Sys.setenv, as.list(restore_values))
    }
    if (length(unset_names) > 0) {
      Sys.unsetenv(unset_names)
    }
  }, add = TRUE)

  values_to_set <- new_values[!is.na(unlist(new_values, use.names = FALSE))]
  values_to_unset <- names(new_values)[is.na(unlist(new_values, use.names = FALSE))]

  if (length(values_to_set) > 0) {
    do.call(Sys.setenv, as.list(values_to_set))
  }
  if (length(values_to_unset) > 0) {
    Sys.unsetenv(values_to_unset)
  }

  force(code)
}

test_that("configured Azure providers include the new enterprise Claude instance", {
  with_env_overrides(
    c(
      AZURE_OPENAI_API_KEY = "primary-key",
      AZURE_OPENAI_ENDPOINT = "https://primary.openai.azure.com/",
      AZURE_OPENAI_DEPLOYMENT = "gpt-5-mini",
      AZURE_OPENAI_API_KEY_NEW = "secondary-key",
      AZURE_OPENAI_ENDPOINT_NEW = "https://secondary.openai.azure.com/",
      AZURE_OPENAI_DEPLOYMENT_NEW = "enterprise-claude"
    ),
    {
      choices <- get_secure_provider_choices()

      expect_named(choices, c(
        "Azure OpenAI (Primary)",
        "Azure OpenAI (Enterprise Claude)"
      ))
      expect_equal(unname(choices), c("azure", "azure_new"))
      expect_true(is_configured_azure_provider("azure_new"))
      expect_true(is_secure_ai_provider("azure_new"))
      expect_equal(get_default_ai_model("azure_new"), "enterprise-claude")
    }
  )
})

test_that("primary Azure is the secure fallback when nothing is configured", {
  with_env_overrides(
    c(
      AZURE_OPENAI_API_KEY = NA_character_,
      AZURE_OPENAI_ENDPOINT = NA_character_,
      AZURE_OPENAI_DEPLOYMENT = NA_character_,
      AZURE_OPENAI_API_KEY_NEW = NA_character_,
      AZURE_OPENAI_ENDPOINT_NEW = NA_character_,
      AZURE_OPENAI_DEPLOYMENT_NEW = NA_character_
    ),
    {
      expect_equal(get_visible_azure_provider_ids(), "azure")
      expect_equal(get_default_secure_provider_id(), "azure")
      expect_equal(get_default_ai_model("azure"), "")
    }
  )
})
