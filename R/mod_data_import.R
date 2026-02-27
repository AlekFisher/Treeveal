# mod_data_import.R
# Data upload sidebar + Data Preview tab

# --- Sidebar UI: file uploads, demo toggle, variable selectors ---
data_import_ui <- function(id) {
  ns <- NS(id)

  tagList(
    fileInput(
      ns("data_file"),
      "Upload Dataset",
      accept = c(".csv", ".CSV", ".xlsx", ".xls", ".sav", ".rds", ".RData", ".rda"),
      placeholder = "CSV, Excel, SPSS, or R files..."
    ),

    fileInput(
      ns("dict_file"),
      "Upload Data Dictionary (Optional)",
      accept = c(".csv", ".CSV", ".xlsx", ".xls"),
      placeholder = "Variable labels & descriptions..."
    ),
    helpText(
      class = "small",
      "Dictionary should have columns: variable, label, and optionally notes"
    ),

    checkboxInput(ns("use_demo"), "Use Demo Dataset (HCP GLP-1 Prescribing)", value = FALSE),

    conditionalPanel(
      condition = "output.data_loaded",
      hr(),
      selectInput(
        ns("outcome_var"),
        "Outcome Variable",
        choices = NULL,
        selected = NULL
      ),
      selectInput(
        ns("predictor_vars"),
        "Predictor Variables",
        choices = NULL,
        selected = NULL,
        multiple = TRUE
      ),
      helpText("Select variables to include in the model"),

      # Prefix removal section
      div(
        class = "mt-2",
        style = "font-size: 0.85em;",
        div(
          class = "d-flex justify-content-between align-items-center mb-1",
          tags$label("Remove by Prefix", class = "form-label small text-muted mb-0"),
          actionButton(
            ns("reset_predictors"),
            "Reset All",
            class = "btn-outline-secondary btn-sm",
            style = "font-size: 0.7em; padding: 1px 6px;",
            icon = icon("rotate-left")
          )
        ),
        uiOutput(ns("prefix_buttons"))
      )
    )
  )
}

# --- Tab UI: Data Preview ---
data_import_tab_ui <- function(id) {
  ns <- NS(id)

  layout_columns(
    col_widths = c(12),

    card(
      card_header("Dataset Overview"),
      card_body(
        conditionalPanel(
          condition = "!output.data_loaded",
          div(
            class = "text-center py-5",
            bsicons::bs_icon("cloud-upload", size = "4rem", class = "text-muted"),
            h4(class = "text-muted mt-3", "Upload a dataset to begin"),
            p(class = "text-muted", "Supported formats: CSV, Excel (.xlsx, .xls), SPSS (.sav), R (.rds, .RData)")
          )
        ),
        conditionalPanel(
          condition = "output.data_loaded",
          layout_columns(
            col_widths = c(3, 3, 3, 3),
            value_box(
              title = "Observations",
              value = textOutput(ns("n_rows"), inline = TRUE),
              showcase = bsicons::bs_icon("people"),
              theme = "primary"
            ),
            value_box(
              title = "Variables",
              value = textOutput(ns("n_cols"), inline = TRUE),
              showcase = bsicons::bs_icon("columns-gap"),
              theme = "secondary"
            ),
            value_box(
              title = "Complete Cases",
              value = textOutput(ns("complete_cases"), inline = TRUE),
              showcase = bsicons::bs_icon("check-circle"),
              theme = "success"
            ),
            value_box(
              title = "Dictionary Labels",
              value = textOutput(ns("n_dict_labels"), inline = TRUE),
              showcase = bsicons::bs_icon("journal-text"),
              theme = "info"
            )
          ),
          hr(),
          DTOutput(ns("data_preview"))
        )
      )
    )
  )
}

# --- Server ---
data_import_server <- function(id, rv) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # --- File Upload ---
    observeEvent(input$data_file, {
      req(input$data_file)

      tryCatch({
        data <- load_data_file(input$data_file$datapath, input$data_file$name)
        file_ext <- tolower(tools::file_ext(input$data_file$name))

        rv$data <- data
        rv$data_dict <- NULL
        rv$model <- NULL
        rv$chat_history <- list()
        rv$chat <- NULL
        rv$factor_levels_pending <- list()
        rv$is_demo_data <- FALSE

        showNotification(
          paste0("Data loaded successfully! (", file_ext, " file, ", nrow(data), " rows)"),
          type = "message"
        )
      }, error = function(e) {
        showNotification(paste("Error loading data:", e$message), type = "error")
      })
    })

    # --- Dictionary Upload ---
    observeEvent(input$dict_file, {
      req(input$dict_file)

      tryCatch({
        dict <- load_dict_file(input$dict_file$datapath, input$dict_file$name)

        rv$data_dict <- dict
        rv$chat <- NULL  # Reset chat when dictionary changes

        showNotification(
          paste0("Dictionary loaded! (", nrow(dict), " variable labels)"),
          type = "message"
        )
      }, error = function(e) {
        showNotification(paste("Error loading dictionary:", e$message), type = "error")
      })
    })

    # --- Demo Data ---
    observeEvent(input$use_demo, {
      if (input$use_demo) {
        demo <- generate_demo_dataset()

        rv$data <- demo$data
        rv$data_dict <- demo$dict
        rv$model <- NULL
        rv$chat_history <- list()
        rv$chat <- NULL
        rv$factor_levels_pending <- list()
        rv$is_demo_data <- TRUE

        showNotification("Demo dataset (HCP GLP-1 Attitudes Survey) loaded!", type = "message")
      }
    })

    # --- Variable Selectors ---
    observe({
      req(rv$data)
      var_names <- names(rv$data)

      updateSelectInput(session, "outcome_var",
        choices = var_names,
        selected = var_names[1]
      )
      updateSelectInput(session, "predictor_vars",
        choices = var_names,
        selected = var_names[-1]
      )
    })

    # Sync selected variables to rv for cross-module access
    observe({
      rv$outcome_var <- input$outcome_var
    })
    observe({
      rv$predictor_vars <- input$predictor_vars
    })

    # --- Prefix Removal Buttons ---
    output$prefix_buttons <- renderUI({
      req(input$predictor_vars)

      vars <- input$predictor_vars
      prefixes <- sapply(vars, function(v) {
        if (grepl("_", v)) {
          sub("_.*", "", v)
        } else if (nchar(v) >= 3) {
          substr(v, 1, 3)
        } else {
          v
        }
      })

      prefix_counts <- table(prefixes)
      multi_prefixes <- names(prefix_counts[prefix_counts >= 2])

      if (length(multi_prefixes) == 0) {
        return(helpText(class = "small text-muted", "No common prefixes detected"))
      }

      multi_prefixes <- multi_prefixes[order(-prefix_counts[multi_prefixes])]

      buttons <- lapply(multi_prefixes, function(prefix) {
        count <- prefix_counts[prefix]
        actionButton(
          inputId = ns(paste0("remove_prefix_", prefix)),
          label = paste0(prefix, "_ (", count, ")"),
          class = "btn-outline-danger btn-sm me-1 mb-1",
          style = "font-size: 0.75em; padding: 2px 6px;"
        )
      })

      tagList(
        div(class = "d-flex flex-wrap", buttons),
        helpText(class = "small text-muted mt-1", "Click to remove all variables with this prefix")
      )
    })

    # Observe prefix removal button clicks
    observe({
      req(rv$data)
      vars <- names(rv$data)

      prefixes <- unique(sapply(vars, function(v) {
        if (grepl("_", v)) sub("_.*", "", v) else if (nchar(v) >= 3) substr(v, 1, 3) else v
      }))

      lapply(prefixes, function(prefix) {
        observeEvent(input[[paste0("remove_prefix_", prefix)]], {
          current_vars <- input$predictor_vars

          pattern <- paste0("^", prefix, "_")
          vars_to_remove <- current_vars[grepl(pattern, current_vars)]

          if (length(vars_to_remove) > 0) {
            new_vars <- setdiff(current_vars, vars_to_remove)
            updateSelectInput(session, "predictor_vars", selected = new_vars)
            showNotification(
              paste0("Removed ", length(vars_to_remove), " variables with prefix '", prefix, "_'"),
              type = "message"
            )
          }
        }, ignoreInit = TRUE)
      })
    })

    # Reset predictors to full list
    observeEvent(input$reset_predictors, {
      req(rv$data)
      all_vars <- names(rv$data)
      outcome <- input$outcome_var
      predictors <- setdiff(all_vars, outcome)
      updateSelectInput(session, "predictor_vars", selected = predictors)
      showNotification("Predictor variables reset to full list", type = "message")
    })

    # --- Data Preview Outputs ---
    output$n_rows <- renderText({
      req(rv$data)
      format(nrow(rv$data), big.mark = ",")
    })

    output$n_cols <- renderText({
      req(rv$data)
      ncol(rv$data)
    })

    output$complete_cases <- renderText({
      req(rv$data)
      pct <- round(sum(complete.cases(rv$data)) / nrow(rv$data) * 100, 1)
      paste0(pct, "%")
    })

    output$n_dict_labels <- renderText({
      if (is.null(rv$data_dict)) {
        "None"
      } else {
        nrow(rv$data_dict)
      }
    })

    output$data_preview <- renderDT({
      req(rv$data)
      datatable(
        rv$data,
        options = list(
          pageLength = 10,
          scrollX = TRUE,
          dom = 'frtip'
        ),
        class = 'compact stripe hover'
      )
    })
  })
}
