# mod_ai_chat.R
# AI Configuration sidebar + Chat card UI + Chat logic

# --- Sidebar UI: provider, model, persona, study context ---
ai_chat_sidebar_ui <- function(id, production_mode = FALSE) {
  ns <- NS(id)

  tagList(
    # Show different provider options based on production mode
    if (production_mode) {
      tagList(
        div(
          class = "alert alert-info",
          bsicons::bs_icon("shield-check"),
          " Production mode: Using secure Azure OpenAI"
        ),
        selectInput(
          ns("ai_provider"),
          "AI Provider",
          choices = c("Azure OpenAI (Secure)" = "azure"),
          selected = "azure"
        )
      )
    } else {
      tagList(
        div(
          class = "alert alert-warning mb-3",
          style = "font-size: 0.85em;",
          bsicons::bs_icon("exclamation-triangle"),
          " Dev mode: Do not use with client data"
        ),
        selectInput(
          ns("ai_provider"),
          "AI Provider",
          choices = c(
            "Azure OpenAI (Secure)" = "azure",
            "Local (Ollama)" = "ollama",
            "Anthropic (Claude)" = "anthropic",
            "Google (Gemini)" = "gemini",
            "OpenAI (GPT)" = "openai"
          ),
          selected = "openai"
        )
      )
    },

    selectInput(
      ns("ai_model"),
      "Model",
      choices = NULL,
      selected = NULL
    ),

    selectInput(
      ns("user_persona"),
      "Response Style",
      choices = c(
        "Executive Summary" = "executive",
        "Project Team (Default)" = "project_team",
        "Statistician" = "statistician"
      ),
      selected = "project_team"
    ),
    helpText(
      class = "small text-muted",
      style = "line-height: 1.3; font-size: 0.8em;",
      tags$strong("Executive:"), " Brief, action-focused", tags$br(),
      tags$strong("Project Team:"), " Clear narrative for reports", tags$br(),
      tags$strong("Statistician:"), " Technical detail & diagnostics"
    ),

    textAreaInput(
      ns("study_context"),
      "Study Context (Optional)",
      placeholder = "Provide any relevant context about your data, research questions, or domain knowledge that would help the AI interpret results...",
      rows = 4
    )
  )
}

# --- Chat Card UI (placed inside tree tab by mod_tree_viz) ---
ai_chat_card_ui <- function(id) {
  ns <- NS(id)

  card(
    card_header(
      class = "d-flex justify-content-between align-items-center",
      span("AI Interpretation"),
      actionButton(
        ns("clear_chat"),
        "Clear",
        class = "btn-sm btn-outline-secondary",
        icon = icon("trash")
      )
    ),
    card_body(
      class = "d-flex flex-column",
      style = "height: 600px; padding: 1rem;",

      conditionalPanel(
        condition = "!output.model_built",
        div(
          class = "text-center py-5",
          bsicons::bs_icon("robot", size = "4rem", class = "text-muted"),
          h4(class = "text-muted mt-3", "Build a model first"),
          p(class = "text-muted", "The AI needs a decision tree to interpret")
        )
      ),

      conditionalPanel(
        condition = "output.model_built",
        class = "d-flex flex-column flex-grow-1",
        style = "min-height: 0;",

        # Chat history display
        div(
          id = "chat_container",
          class = "flex-grow-1",
          style = "overflow-y: auto; border: 1px solid #e8e5f5; border-radius: 12px; padding: 15px; background: linear-gradient(180deg, #f1f0fb, #f8f7ff); min-height: 0;",
          uiOutput(ns("chat_history"))
        ),

        # Quick action buttons
        div(
          class = "mt-2 mb-2 flex-shrink-0",
          actionButton(ns("ask_interpret"), "Interpret", class = "btn-outline-primary btn-sm me-1", icon = icon("lightbulb")),
          actionButton(ns("ask_insights"), "Insights", class = "btn-outline-primary btn-sm me-1", icon = icon("chart-line")),
          actionButton(ns("ask_recommendations"), "Recommend", class = "btn-outline-primary btn-sm me-1", icon = icon("list-check")),
          actionButton(ns("ask_limitations"), "Limits", class = "btn-outline-primary btn-sm", icon = icon("exclamation-triangle"))
        ),

        # User input
        div(
          class = "flex-shrink-0",
          layout_columns(
            col_widths = c(9, 3),
            textAreaInput(
              ns("user_message"),
              NULL,
              placeholder = "Ask about your tree...",
              rows = 2
            ),
            actionButton(
              ns("send_message"),
              "Send",
              class = "btn-primary w-100 h-100",
              icon = icon("paper-plane")
            )
          ),
          p(
            class = "text-muted small mt-2 mb-0",
            style = "font-size: 0.75rem; line-height: 1.3;",
            bsicons::bs_icon("info-circle", class = "me-1"),
            "AI can make mistakes. Always verify interpretations against the actual model output and use professional judgment."
          )
        )
      )
    )
  )
}

# --- Server ---
ai_chat_server <- function(id, rv, production_mode) {
  moduleServer(id, function(input, output, session) {

    # --- Update AI model choices based on provider ---
    observeEvent(input$ai_provider, {
      rv$chat <- NULL

      if (input$ai_provider == "azure") {
        azure_model <- Sys.getenv("AZURE_OPENAI_DEPLOYMENT")
        if (azure_model == "") azure_model <- "gpt-5-mini"
        updateSelectInput(
          session, "ai_model",
          choices = setNames(azure_model, paste0("Azure: ", azure_model)),
          selected = azure_model
        )
      } else if (input$ai_provider == "ollama") {
        updateSelectInput(
          session, "ai_model",
          choices = c("Ministral 3 (8B)" = "ministral-3:8b"),
          selected = "ministral-3:8b"
        )
      } else if (input$ai_provider == "anthropic") {
        updateSelectInput(
          session, "ai_model",
          choices = c(
            "claude-haiku-4-5-20251001" = "claude-haiku-4-5-20251001",
            "claude-sonnet-4-20250514" = "claude-sonnet-4-20250514",
            "claude-opus-4-20250514" = "claude-opus-4-20250514"
          ),
          selected = "claude-haiku-4-5-20251001"
        )
      } else if (input$ai_provider == "gemini") {
        updateSelectInput(
          session, "ai_model",
          choices = c("gemini-3-flash-preview" = "gemini-3-flash-preview"),
          selected = "gemini-3-flash-preview"
        )
      } else {
        updateSelectInput(
          session, "ai_model",
          choices = c(
            "gpt-5-mini" = "gpt-5-mini",
            "gpt-4o" = "gpt-4o",
            "gpt-4o-mini" = "gpt-4o-mini"
          ),
          selected = "gpt-5-mini"
        )
      }
    })

    # Reset chat when persona changes
    observeEvent(input$user_persona, {
      rv$chat <- NULL
    })

    # --- Dictionary Context ---
    get_dictionary_context <- reactive({
      if (is.null(rv$data_dict)) return(NULL)

      dict <- rv$data_dict
      dict_lines <- sapply(seq_len(nrow(dict)), function(i) {
        line <- paste0("- ", dict$variable[i], ": ", dict$label[i])
        if (!is.na(dict$notes[i]) && dict$notes[i] != "") {
          line <- paste0(line, " [Note: ", dict$notes[i], "]")
        }
        line
      })

      paste0(
        "\n\nDATA DICTIONARY\n",
        "===============\n",
        "The following variable labels explain what each variable measures:\n\n",
        paste(dict_lines, collapse = "\n")
      )
    })

    # --- Model Context ---
    get_model_context <- reactive({
      req(rv$model, rv$data, rv$outcome_var)

      rules <- capture.output(rpart.plot::rpart.rules(rv$model, style = "tall", cover = TRUE))

      var_imp <- if (!is.null(rv$model$variable.importance)) {
        var_names <- names(rv$model$variable.importance)
        var_scores <- round(rv$model$variable.importance, 2)

        if (!is.null(rv$data_dict)) {
          var_labels <- sapply(var_names, function(v) {
            match_idx <- match(v, rv$data_dict$variable)
            if (!is.na(match_idx)) {
              paste0(v, " (", rv$data_dict$label[match_idx], ")")
            } else {
              v
            }
          })
          paste(var_labels, ":", var_scores, collapse = "\n")
        } else {
          paste(var_names, ":", var_scores, collapse = "\n")
        }
      } else {
        "Not available"
      }

      n_nodes <- sum(rv$model$frame$var == "<leaf>")

      cm_text <- ""
      if (rv$model$method == "class") {
        pred <- predict(rv$model, type = "class")
        actual <- rv$data[[rv$outcome_var]]
        cm <- table(Predicted = pred, Actual = actual)
        accuracy <- sum(diag(cm)) / sum(cm)
        cm_text <- paste0(
          "\n\nConfusion Matrix:\n",
          capture.output(print(cm)) |> paste(collapse = "\n"),
          "\n\nAccuracy: ", round(accuracy * 100, 1), "%"
        )
      }

      dict_context <- get_dictionary_context()

      paste0(
        "DECISION TREE MODEL SUMMARY\n",
        "===========================\n\n",
        "Outcome Variable: ", rv$outcome_var, "\n",
        "Model Type: ", ifelse(rv$model$method == "class", "Classification", "Regression"), "\n",
        "Number of Terminal Nodes: ", n_nodes, "\n",
        "Training Observations: ", nrow(rv$data), "\n",
        "\nVariable Importance:\n", var_imp, "\n",
        "\nDecision Rules:\n", paste(rules, collapse = "\n"),
        cm_text,
        "\n\nModel Parameters:\n",
        "- Complexity Parameter (cp): ", rv$cp, "\n",
        "- Minimum Bucket Size: ", rv$minbucket, "\n",
        "- Maximum Depth: ", rv$maxdepth,
        if (!is.null(dict_context)) dict_context else ""
      )
    })

    # --- Initialize or get chat object ---
    get_chat <- function() {
      if (is.null(rv$chat)) {

        persona_instructions <- switch(input$user_persona,
          "executive" = paste0(
            "\n\nRESPONSE STYLE - EXECUTIVE SUMMARY:\n",
            "- Be extremely concise - aim for 3-5 bullet points maximum\n",
            "- Lead with the single most important finding\n",
            "- Focus on 'so what?' - business implications and recommended actions\n",
            "- Avoid statistical jargon entirely - use plain business language\n",
            "- Use specific numbers only when they drive a decision (e.g., '81% of high prescribers...')\n",
            "- Format: Brief intro sentence, then bullet points, then one-line recommendation\n",
            "- Total response should be readable in under 30 seconds\n",
            "- Do NOT offer to run additional analyses or suggest follow-up statistical work"
          ),
          "project_team" = paste0(
            "\n\nRESPONSE STYLE - PROJECT TEAM:\n",
            "- Write clear, flowing paragraphs suitable for a client report\n",
            "- Explain findings in accessible language - assume smart non-statisticians\n",
            "- Include key numbers and percentages that tell the story\n",
            "- Connect findings to practical implications\n",
            "- Use 2-3 short paragraphs, not lengthy technical explanations\n",
            "- Avoid statistical terminology unless you briefly explain it\n",
            "- The output should be something they could paste into a PowerPoint or report\n",
            "- Do NOT end with offers to run additional analyses - you cannot execute code or run new models in this tool\n",
            "- If relevant, you may mention what additional analyses COULD be done outside this tool, but frame it as 'for further investigation, your analyst could...' rather than offering to do it yourself"
          ),
          "statistician" = paste0(
            "\n\nRESPONSE STYLE - STATISTICIAN:\n",
            "- Include full technical detail: accuracy metrics, node counts, split criteria\n",
            "- Discuss model diagnostics: potential overfitting, variable importance rankings\n",
            "- Reference specific threshold values and their statistical meaning\n",
            "- Compare decision tree results with Random Forest importance where relevant\n",
            "- Note methodological considerations and limitations\n",
            "- Use proper statistical terminology\n",
            "- IMPORTANT: You can only interpret the model that has already been built - you cannot run additional analyses, execute code, or build new models\n",
            "- If suggesting additional analyses (cross-validation, sensitivity analysis, etc.), clearly frame these as recommendations for the analyst to pursue separately, NOT something you can do in this conversation\n",
            "- For example, say 'I would recommend validating with k-fold CV' rather than 'I can compute cross-validated performance'"
          ),
          paste0(
            "\n\nRESPONSE STYLE:\n",
            "- Be clear and concise\n",
            "- Use plain language\n",
            "- Focus on actionable insights\n",
            "- Do not offer to run additional analyses"
          )
        )

        system_prompt <- paste0(
          "You are an expert statistical analyst and data scientist specializing in decision tree analysis. ",
          "You are helping a user understand and interpret their decision tree model.\n\n",
          "Here is the context about the current decision tree model:\n\n",
          get_model_context(),
          if (nzchar(input$study_context)) {
            paste0("\n\nAdditional Study Context from User:\n", input$study_context)
          } else {
            ""
          },
          "\n\nIMPORTANT INSTRUCTIONS:\n",
          "- When discussing variables, ALWAYS use their descriptive labels (from the data dictionary) rather than raw variable names.\n",
          "- For example, say 'belief that early effective treatment leads to best outcomes' instead of 'a0_7'.\n",
          "- Provide clear, actionable insights. Use specific numbers from the model when relevant.\n",
          "- Be conversational but precise. If the user asks about something not shown in the model, ",
          "explain what additional analysis might be needed.",
          persona_instructions
        )

        # Create chat based on provider
        if (input$ai_provider == "azure") {
          model <- input$ai_model
          if (model == "" || is.null(model)) {
            model <- Sys.getenv("AZURE_OPENAI_DEPLOYMENT")
          }
          api_key <- Sys.getenv("AZURE_OPENAI_API_KEY")
          endpoint <- Sys.getenv("AZURE_OPENAI_ENDPOINT")

          missing_vars <- c()
          if (api_key == "") missing_vars <- c(missing_vars, "AZURE_OPENAI_API_KEY")
          if (endpoint == "") missing_vars <- c(missing_vars, "AZURE_OPENAI_ENDPOINT")
          if (model == "") missing_vars <- c(missing_vars, "AZURE_OPENAI_DEPLOYMENT")

          if (length(missing_vars) > 0) {
            stop(paste0(
              "Azure OpenAI is not configured. Missing environment variables:\n\n",
              paste("\u2022", missing_vars, collapse = "\n"),
              "\n\nPlease contact your administrator to set up Azure OpenAI access, ",
              "or switch to a different AI provider in the sidebar."
            ))
          }

          rv$chat <- ellmer::chat_azure_openai(
            endpoint = endpoint,
            model = model,
            api_key = api_key,
            api_version = "2025-04-01-preview",
            system_prompt = system_prompt,
            echo = FALSE
          )

        } else if (input$ai_provider == "ollama") {
          rv$chat <- tryCatch({
            ellmer::chat_ollama(
              model = input$ai_model,
              system_prompt = system_prompt
            )
          }, error = function(e) {
            stop(paste0(
              "Could not connect to Ollama. Please ensure:\n\n",
              "\u2022 Ollama is installed and running locally\n",
              "\u2022 The model '", input$ai_model, "' is downloaded\n",
              "\u2022 Ollama is accessible at http://localhost:11434\n\n",
              "Error details: ", e$message
            ))
          })

        } else if (input$ai_provider == "anthropic") {
          api_key <- Sys.getenv("ANTHROPIC_API_KEY")
          if (api_key == "") {
            stop(paste0(
              "Anthropic API key not found.\n\n",
              "Please set the ANTHROPIC_API_KEY environment variable.\n\n",
              "You can get an API key from: https://console.anthropic.com/"
            ))
          }
          rv$chat <- ellmer::chat_anthropic(
            model = input$ai_model,
            system_prompt = system_prompt
          )

        } else if (input$ai_provider == "gemini") {
          api_key <- Sys.getenv("GOOGLE_API_KEY")
          if (api_key == "") {
            stop(paste0(
              "Google Gemini API key not found.\n\n",
              "Please set the GOOGLE_API_KEY environment variable.\n\n",
              "You can get an API key from: https://aistudio.google.com/apikey"
            ))
          }
          rv$chat <- ellmer::chat_google_gemini(
            model = input$ai_model,
            system_prompt = system_prompt,
            api_key = api_key
          )

        } else if (input$ai_provider == "openai") {
          api_key <- Sys.getenv("OPENAI_API_KEY")
          if (api_key == "") {
            stop(paste0(
              "OpenAI API key not found.\n\n",
              "Please set the OPENAI_API_KEY environment variable.\n\n",
              "You can get an API key from: https://platform.openai.com/api-keys"
            ))
          }
          rv$chat <- ellmer::chat_openai(
            model = input$ai_model,
            system_prompt = system_prompt
          )
        }
      }

      rv$chat
    }

    # --- Send message to AI ---
    send_to_ai <- function(message) {
      req(rv$model)

      rv$chat_history <- c(rv$chat_history, list(
        list(role = "user", content = message)
      ))

      chat <- tryCatch({
        get_chat()
      }, error = function(e) {
        rv$chat_history <- c(rv$chat_history, list(
          list(role = "assistant", content = paste0(
            "\u26a0\ufe0f **AI Configuration Error**\n\n",
            e$message,
            "\n\nPlease check your settings and try again."
          ))
        ))
        return(NULL)
      })

      if (is.null(chat)) return(invisible(NULL))

      tryCatch({
        response <- chat$chat(message)

        rv$chat_history <- c(rv$chat_history, list(
          list(role = "assistant", content = response)
        ))

      }, error = function(e) {
        error_text <- e$message

        user_message <- if (grepl("401|unauthorized|invalid.*key", error_text, ignore.case = TRUE)) {
          "**Authentication Failed**\n\nYour API key appears to be invalid or expired. Please check your API key configuration."
        } else if (grepl("429|rate.?limit|too many requests", error_text, ignore.case = TRUE)) {
          "**Rate Limit Exceeded**\n\nToo many requests. Please wait a moment and try again."
        } else if (grepl("timeout|timed out|connection", error_text, ignore.case = TRUE)) {
          "**Connection Error**\n\nCould not connect to the AI service. Please check your internet connection and try again."
        } else if (grepl("500|502|503|504|server error", error_text, ignore.case = TRUE)) {
          "**Service Unavailable**\n\nThe AI service is temporarily unavailable. Please try again in a few minutes."
        } else if (grepl("model.*not found|does not exist", error_text, ignore.case = TRUE)) {
          paste0("**Model Not Found**\n\nThe selected model '", input$ai_model, "' is not available. Please select a different model.")
        } else {
          paste0("**Error**\n\n", error_text)
        }

        rv$chat_history <- c(rv$chat_history, list(
          list(role = "assistant", content = paste0("\u26a0\ufe0f ", user_message))
        ))

        showNotification(
          "AI request failed. See chat for details.",
          type = "error",
          duration = 5
        )
      })
    }

    # --- Chat Event Handlers ---
    observeEvent(input$send_message, {
      req(input$user_message, nzchar(trimws(input$user_message)))
      message <- trimws(input$user_message)
      updateTextAreaInput(session, "user_message", value = "")
      withProgress(message = "Thinking...", {
        send_to_ai(message)
      })
    })

    observeEvent(input$ask_interpret, {
      withProgress(message = "Analyzing...", {
        send_to_ai("Please provide a comprehensive interpretation of this decision tree. Explain the key decision paths, what variables are most important, and what the tree tells us about the outcome variable.")
      })
    })

    observeEvent(input$ask_insights, {
      withProgress(message = "Extracting insights...", {
        send_to_ai("What are the 3-5 most important insights from this decision tree? Focus on actionable findings that would be valuable for decision-making.")
      })
    })

    observeEvent(input$ask_recommendations, {
      withProgress(message = "Generating recommendations...", {
        send_to_ai("Based on this decision tree analysis, what recommendations would you make? Consider both the model findings and potential next steps for analysis or action.")
      })
    })

    observeEvent(input$ask_limitations, {
      withProgress(message = "Assessing limitations...", {
        send_to_ai("What are the potential limitations of this decision tree model? Consider issues like overfitting, sample size, variable selection, and generalizability.")
      })
    })

    observeEvent(input$clear_chat, {
      rv$chat_history <- list()
      rv$chat <- NULL
      showNotification("Chat cleared", type = "message")
    })

    # --- Chat Display ---
    output$chat_history <- renderUI({
      if (length(rv$chat_history) == 0) {
        return(
          div(
            class = "text-center text-muted py-5",
            bsicons::bs_icon("chat-dots", size = "2rem"),
            p(class = "mt-2", "Start a conversation or use the quick action buttons above")
          )
        )
      }

      chat_items <- lapply(rv$chat_history, function(msg) {
        if (msg$role == "user") {
          div(
            class = "d-flex justify-content-end mb-3",
            div(
              class = "p-3 rounded-3",
              style = "background: linear-gradient(135deg, #6366f1, #4f46e5); color: white; max-width: 80%;",
              HTML(markdown::markdownToHTML(
                text = msg$content,
                fragment.only = TRUE
              ))
            )
          )
        } else {
          div(
            class = "d-flex justify-content-start mb-3",
            div(
              class = "p-3 rounded-3",
              style = "background-color: white; border: 1px solid #e8e5f5; border-left: 3px solid #a5b4fc; max-width: 80%;",
              HTML(markdown::markdownToHTML(
                text = msg$content,
                fragment.only = TRUE
              ))
            )
          )
        }
      })

      tagList(chat_items)
    })
  })
}
