# Treeveal: AI-Powered Decision Tree Analysis
# An interactive Shiny application for building, visualizing, and interpreting decision trees
# with AI assistance via ellmer

library(shiny)
library(bslib)
library(rpart)
library(rpart.plot)
library(ellmer)
library(DT)
library(dplyr)
library(tidyr)
library(ggplot2)
library(readxl)
library(haven)

# ============================================================================
# PRODUCTION MODE TOGGLE
# ============================================================================
# Set to TRUE for production (client data) - only Azure OpenAI will be available
# Set to FALSE for testing/development - all AI providers will be available
PRODUCTION_MODE <- FALSE

# ============================================================================
# UI
# ============================================================================

ui <- page_sidebar(
  title = tags$span(
    "Treeveal",
    # Show production mode indicator in title
    if (PRODUCTION_MODE) {
      tags$span(
        class = "badge bg-success ms-2",
        style = "font-size: 0.6em; vertical-align: middle;",
        "PRODUCTION"
      )
    } else {
      tags$span(
        class = "badge bg-warning ms-2",
        style = "font-size: 0.6em; vertical-align: middle;",
        "DEV"
      )
    }
  ),
  theme = bs_theme(
    version = 5,
    bootswatch = "default",
    primary = "#0071e3",
    secondary = "#86868b",
    success = "#34c759",
    info = "#5ac8fa",
    warning = "#ff9f0a",
    danger = "#ff3b30",
    base_font = font_google("Inter"),
    heading_font = font_google("Inter"),
    code_font = font_google("JetBrains Mono"),
    "body-bg" = "#f5f5f7",
    "card-bg" = "#ffffff",
    "border-radius" = "12px"
  ) |> bs_add_rules(sass::sass_file("www/styles.css")),

  # Sidebar with data and model controls
  sidebar = sidebar(
    width = 350,
    title = "Configuration",

    # Data Upload Section
    accordion(
      id = "sidebar_accordion",
      open = c("data_panel", "model_panel"),

      accordion_panel(
        title = "Data Upload",
        value = "data_panel",
        icon = bsicons::bs_icon("cloud-upload"),

        fileInput(
          "data_file",
          "Upload Dataset",
          accept = c(".csv", ".CSV", ".xlsx", ".xls", ".sav", ".rds", ".RData", ".rda"),
          placeholder = "CSV, Excel, SPSS, or R files..."
        ),

        fileInput(
          "dict_file",
          "Upload Data Dictionary (Optional)",
          accept = c(".csv", ".CSV", ".xlsx", ".xls"),
          placeholder = "Variable labels & descriptions..."
        ),
        helpText(
          class = "small",
          "Dictionary should have columns: variable, label, and optionally notes"
        ),

        checkboxInput("use_demo", "Use Demo Dataset (HCP GLP-1 Prescribing)", value = FALSE),

        conditionalPanel(
          condition = "output.data_loaded",
          hr(),
          selectInput(
            "outcome_var",
            "Outcome Variable",
            choices = NULL,
            selected = NULL
          ),
          selectInput(
            "predictor_vars",
            "Predictor Variables",
            choices = NULL,
            selected = NULL,
            multiple = TRUE
          ),
          helpText("Select variables to include in the model")
        )
      ),

      accordion_panel(
        title = "Model Parameters",
        value = "model_panel",
        icon = bsicons::bs_icon("sliders"),

        sliderInput(
          "cp",
          "Complexity Parameter (cp)",
          min = 0.001,
          max = 0.1,
          value = 0.01,
          step = 0.001
        ),
        helpText("Lower values = more complex trees"),

        sliderInput(
          "minbucket",
          "Minimum Bucket Size",
          min = 1,
          max = 50,
          value = 30,
          step = 1
        ),
        helpText("Minimum observations in terminal nodes"),

        sliderInput(
          "maxdepth",
          "Maximum Depth",
          min = 1,
          max = 30,
          value = 4,
          step = 1
        ),

        hr(),

        actionButton(
          "run_model",
          "Build Decision Tree",
          class = "btn-primary btn-lg w-100",
          icon = icon("play")
        )
      ),

      accordion_panel(
        title = "AI Configuration",
        value = "ai_panel",
        icon = bsicons::bs_icon("robot"),

        # Show different provider options based on production mode
        if (PRODUCTION_MODE) {
          # Production mode: Only Azure OpenAI
          tagList(
            div(
              class = "alert alert-info",
              bsicons::bs_icon("shield-check"),
              " Production mode: Using secure Azure OpenAI"
            ),
            selectInput(
              "ai_provider",
              "AI Provider",
              choices = c("Azure OpenAI (Secure)" = "azure"),
              selected = "azure"
            )
          )
        } else {
          # Development mode: All providers available
          tagList(
            div(
              class = "alert alert-warning mb-3",
              style = "font-size: 0.85em;",
              bsicons::bs_icon("exclamation-triangle"),
              " Dev mode: Do not use with client data"
            ),
            selectInput(
              "ai_provider",
              "AI Provider",
              choices = c(
                "Azure OpenAI (Secure)" = "azure",
                "Local (Ollama)" = "ollama",
                "Anthropic (Claude)" = "anthropic",
                "Google (Gemini)" = "gemini",
                "OpenAI (GPT)" = "openai"
              ),
              selected = "azure"
            )
          )
        },

        selectInput(
          "ai_model",
          "Model",
          choices = NULL,
          selected = NULL
        ),

        textAreaInput(
          "study_context",
          "Study Context (Optional)",
          placeholder = "Provide any relevant context about your data, research questions, or domain knowledge that would help the AI interpret results...",
          rows = 4
        )
      )
    )
  ),

  # Main content area
  navset_card_tab(
    id = "main_tabs",

    # Data Preview Tab
    nav_panel(
      title = "Data Preview",
      icon = bsicons::bs_icon("table"),

      layout_columns(
        col_widths = c(12),

        card(
          card_header(
            "Dataset Overview"
          ),
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
                  value = textOutput("n_rows", inline = TRUE),
                  showcase = bsicons::bs_icon("people"),
                  theme = "primary"
                ),
                value_box(
                  title = "Variables",
                  value = textOutput("n_cols", inline = TRUE),
                  showcase = bsicons::bs_icon("columns-gap"),
                  theme = "secondary"
                ),
                value_box(
                  title = "Complete Cases",
                  value = textOutput("complete_cases", inline = TRUE),
                  showcase = bsicons::bs_icon("check-circle"),
                  theme = "success"
                ),
                value_box(
                  title = "Dictionary Labels",
                  value = textOutput("n_dict_labels", inline = TRUE),
                  showcase = bsicons::bs_icon("journal-text"),
                  theme = "info"
                )
              ),
              hr(),
              DTOutput("data_preview")
            )
          )
        )
      )
    ),

    # Decision Tree Tab (with AI Chat)
    nav_panel(
      title = "Decision Tree",
      icon = bsicons::bs_icon("diagram-3"),

      layout_columns(
        col_widths = c(7, 5),

        # Left: Tree Visualization
        card(
          card_header(
            "Decision Tree Visualization"
          ),
          card_body(
            min_height = "600px",
            conditionalPanel(
              condition = "!output.model_built",
              div(
                class = "text-center py-5",
                bsicons::bs_icon("diagram-3", size = "4rem", class = "text-muted"),
                h4(class = "text-muted mt-3", "No model built yet"),
                p(class = "text-muted", "Configure your model and click 'Build Decision Tree'")
              )
            ),
            conditionalPanel(
              condition = "output.model_built",
              plotOutput("tree_plot", height = "550px")
            )
          )
        ),

        # Right: AI Chat
        card(
          card_header(
            class = "d-flex justify-content-between align-items-center",
            span("AI Interpretation"),
            actionButton(
              "clear_chat",
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

              # Chat history display - takes up remaining space
              div(
                id = "chat_container",
                class = "flex-grow-1",
                style = "overflow-y: auto; border: 1px solid #dee2e6; border-radius: 8px; padding: 15px; background-color: #f8f9fa; min-height: 0;",
                uiOutput("chat_history")
              ),

              # Quick action buttons - fixed at bottom
              div(
                class = "mt-2 mb-2 flex-shrink-0",
                actionButton("ask_interpret", "Interpret", class = "btn-outline-primary btn-sm me-1", icon = icon("lightbulb")),
                actionButton("ask_insights", "Insights", class = "btn-outline-primary btn-sm me-1", icon = icon("chart-line")),
                actionButton("ask_recommendations", "Recommend", class = "btn-outline-primary btn-sm me-1", icon = icon("list-check")),
                actionButton("ask_limitations", "Limits", class = "btn-outline-primary btn-sm", icon = icon("exclamation-triangle"))
              ),

              # User input - fixed at bottom
              div(
                class = "flex-shrink-0",
                layout_columns(
                  col_widths = c(9, 3),
                  textAreaInput(
                    "user_message",
                    NULL,
                    placeholder = "Ask about your tree...",
                    rows = 2
                  ),
                  actionButton(
                    "send_message",
                    "Send",
                    class = "btn-primary w-100 h-100",
                    icon = icon("paper-plane")
                  )
                ),
                # AI Disclaimer
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
      )
    ),

    # Model Details Tab
    nav_panel(
      title = "Model Details",
      icon = bsicons::bs_icon("file-earmark-code"),

      layout_columns(
        col_widths = c(6, 6),

        # Left column
        layout_columns(
          col_widths = c(12),

          card(
            card_header("Model Statistics"),
            card_body(
              conditionalPanel(
                condition = "output.model_built",
                verbatimTextOutput("model_stats")
              ),
              conditionalPanel(
                condition = "!output.model_built",
                div(class = "text-center py-3 text-muted", "Build a model to see statistics")
              )
            )
          ),

          card(
            card_header("Variable Importance"),
            card_body(
              conditionalPanel(
                condition = "output.model_built",
                plotOutput("var_importance", height = "200px")
              ),
              conditionalPanel(
                condition = "!output.model_built",
                div(class = "text-center py-3 text-muted", "Build a model to see importance")
              )
            )
          ),

          card(
            card_header("Confusion Matrix"),
            card_body(
              conditionalPanel(
                condition = "output.model_built",
                DTOutput("confusion_matrix")
              ),
              conditionalPanel(
                condition = "!output.model_built",
                div(class = "text-center py-3 text-muted", "Build a classification model to see matrix")
              )
            )
          )
        ),

        # Right column
        layout_columns(
          col_widths = c(12),

          card(
            card_header("Tree Rules"),
            card_body(
              verbatimTextOutput("tree_rules")
            )
          ),

          card(
            card_header("CP Table"),
            card_body(
              DTOutput("cp_table")
            )
          )
        )
      )
    ),

    # Guide & Glossary Tab
    nav_panel(
      title = "Guide",
      icon = bsicons::bs_icon("book"),

      card(
        card_body(
          class = "p-4",

          div(
            class = "mb-4",
            h4("Treeveal User Guide"),
            p(class = "text-muted", "Everything you need to know about decision trees and how to use this app.")
          ),

          accordion(
            id = "guide_accordion",
            open = FALSE,

            # What is a Decision Tree
            accordion_panel(
              title = "What is a Decision Tree?",
              value = "what_is",
              icon = bsicons::bs_icon("diagram-3"),

              p(
                class = "lead",
                "A decision tree is a visual tool that shows how different factors lead to an outcome.
                Think of it like a flowchart that asks yes/no questions to arrive at a prediction."
              ),
              h6("A Simple Example"),
              p(
                "Imagine predicting whether an HCP is a high prescriber of GLP-1 medications.
                The tree might work like this:"
              ),
              div(
                class = "p-3 my-3",
                style = "background: var(--bg-secondary); border-radius: var(--radius-md); border-left: 4px solid var(--color-accent);",
                tags$ol(
                  tags$li("Is the physician comfortable initiating GLP-1s? (rating ≥ 4)"),
                  tags$ul(
                    tags$li(tags$strong("Yes →"), " Are they an Endocrinologist?"),
                    tags$ul(
                      tags$li(tags$strong("Yes →"), " Likely a High Prescriber (85% probability)"),
                      tags$li(tags$strong("No →"), " Check if they believe in early intervention...")
                    ),
                    tags$li(tags$strong("No →"), " Likely a Low Prescriber (72% probability)")
                  )
                )
              ),
              p(
                "Each \"branch\" represents a question about the data. The tree automatically finds
                the questions that best separate high prescribers from low prescribers."
              )
            ),

            # When to Use
            accordion_panel(
              title = "When to Use Decision Trees",
              value = "when_to_use",
              icon = bsicons::bs_icon("check-circle"),

              layout_columns(
                col_widths = c(6, 6),

                div(
                  h6(class = "text-success", "Great For"),
                  tags$ul(
                    tags$li(tags$strong("Segmentation:"), " Understanding what differentiates groups"),
                    tags$li(tags$strong("Identifying key drivers:"), " Finding which factors matter most"),
                    tags$li(tags$strong("Creating actionable rules:"), " \"If X and Y, then likely Z\""),
                    tags$li(tags$strong("Communicating findings:"), " Visual output stakeholders understand"),
                    tags$li(tags$strong("Exploratory analysis:"), " Discovering unexpected patterns")
                  )
                ),

                div(
                  h6(class = "text-danger", "Limitations"),
                  tags$ul(
                    tags$li(tags$strong("Overfitting risk:"), " Trees can memorize noise if too complex"),
                    tags$li(tags$strong("Instability:"), " Small data changes can produce different trees"),
                    tags$li(tags$strong("Linear relationships:"), " May miss smooth, gradual effects"),
                    tags$li(tags$strong("Sample size:"), " Needs enough data in each segment")
                  )
                )
              )
            ),

            # How to Use This App
            accordion_panel(
              title = "How to Use This App",
              value = "how_to_use",
              icon = bsicons::bs_icon("play-circle"),

              div(
                class = "step-item d-flex mb-3",
                div(class = "step-number me-3", "1"),
                div(
                  tags$strong("Upload Your Data"),
                  p(class = "text-muted mb-0 small", "Use a CSV file or try the demo dataset to explore.")
                )
              ),

              div(
                class = "step-item d-flex mb-3",
                div(class = "step-number me-3", "2"),
                div(
                  tags$strong("Select Your Variables"),
                  p(class = "text-muted mb-0 small",
                    "Outcome = what you're predicting. Predictors = factors that might influence it.")
                )
              ),

              div(
                class = "step-item d-flex mb-3",
                div(class = "step-number me-3", "3"),
                div(
                  tags$strong("Adjust Parameters (Optional)"),
                  p(class = "text-muted mb-0 small", "Defaults work well. Adjust if tree is too simple or complex.")
                )
              ),

              div(
                class = "step-item d-flex",
                div(class = "step-number me-3", "4"),
                div(
                  tags$strong("Build & Interpret"),
                  p(class = "text-muted mb-0 small", "Click 'Build Decision Tree' and use the AI to help interpret.")
                )
              )
            ),

            # Parameter Guide
            accordion_panel(
              title = "Parameter Guide",
              value = "parameters",
              icon = bsicons::bs_icon("sliders"),

              tags$table(
                class = "table table-sm",
                tags$thead(
                  tags$tr(
                    tags$th("Parameter"),
                    tags$th("What It Does"),
                    tags$th("When to Adjust")
                  )
                ),
                tags$tbody(
                  tags$tr(
                    tags$td(tags$strong("Complexity (cp)")),
                    tags$td("Controls tree simplicity. Lower = more complex."),
                    tags$td("Raise if too complex, lower if too simple.")
                  ),
                  tags$tr(
                    tags$td(tags$strong("Min Bucket")),
                    tags$td("Minimum observations in final groups."),
                    tags$td("Raise for more reliable segments.")
                  ),
                  tags$tr(
                    tags$td(tags$strong("Max Depth")),
                    tags$td("Maximum levels of questions."),
                    tags$td("Lower (3-5) for simpler trees.")
                  )
                )
              ),
              div(
                class = "alert alert-info mt-2 small",
                bsicons::bs_icon("lightbulb", class = "me-2"),
                "Start with defaults. Adjust only if your tree has 1-2 splits (lower cp) or 20+ splits (raise cp)."
              )
            ),

            # How to Read the Tree
            accordion_panel(
              title = "How to Read the Tree",
              value = "reading",
              icon = bsicons::bs_icon("eye"),

              p("Each box in the tree contains:"),
              tags$ul(
                tags$li(tags$strong("Top line:"), " The predicted class (e.g., \"High\" or \"Low\")"),
                tags$li(tags$strong("Percentages:"), " Probability of each class"),
                tags$li(tags$strong("Bottom %:"), " What portion of data is in this node")
              ),

              h6(class = "mt-3", "Reading Splits"),
              tags$ul(
                tags$li(tags$strong("Yes/Left:"), " Observations meeting the condition go left"),
                tags$li(tags$strong("No/Right:"), " Observations NOT meeting the condition go right")
              ),

              div(
                class = "alert alert-warning mt-2 small",
                bsicons::bs_icon("info-circle", class = "me-2"),
                "Follow a path top-to-bottom to describe a segment: \"HCPs who are comfortable initiating AND believe in early intervention have 85% probability of being high prescribers.\""
              )
            ),

            # Glossary
            accordion_panel(
              title = "Glossary",
              value = "glossary",
              icon = bsicons::bs_icon("journal-text"),

              div(
                class = "row",
                div(
                  class = "col-md-6",
                  tags$dl(
                    class = "small",
                    tags$dt("Accuracy"),
                    tags$dd(class = "text-muted", "Percentage of correct predictions."),

                    tags$dt("Classification Tree"),
                    tags$dd(class = "text-muted", "Predicts categories (High/Low, Yes/No)."),

                    tags$dt("Confusion Matrix"),
                    tags$dd(class = "text-muted", "Table of correct vs incorrect predictions."),

                    tags$dt("CP (Complexity Parameter)"),
                    tags$dd(class = "text-muted", "Controls tree pruning. Higher = simpler tree."),

                    tags$dt("Leaf Node"),
                    tags$dd(class = "text-muted", "Final box with prediction and segment size."),

                    tags$dt("Overfitting"),
                    tags$dd(class = "text-muted", "Tree too complex, learns noise not patterns.")
                  )
                ),
                div(
                  class = "col-md-6",
                  tags$dl(
                    class = "small",
                    tags$dt("Predictor Variable"),
                    tags$dd(class = "text-muted", "Factors used to make predictions."),

                    tags$dt("Pruning"),
                    tags$dd(class = "text-muted", "Removing branches to simplify the tree."),

                    tags$dt("Root Node"),
                    tags$dd(class = "text-muted", "First split; most important variable."),

                    tags$dt("Split"),
                    tags$dd(class = "text-muted", "Decision point dividing data by a variable."),

                    tags$dt("Variable Importance"),
                    tags$dd(class = "text-muted", "Score of each variable's contribution.")
                  )
                )
              )
            )
          )
        )
      )
    )
  )
)

# ============================================================================
# Server
# ============================================================================

server <- function(input, output, session) {

  # -------------------------------------------------------------------------
  # Reactive Values
  # -------------------------------------------------------------------------

  rv <- reactiveValues(
    data = NULL,
    data_dict = NULL,
    model = NULL,
    chat_history = list(),
    chat = NULL  # ellmer chat object
  )

  # -------------------------------------------------------------------------
  # Data Loading
  # -------------------------------------------------------------------------

  # Load uploaded data
  observeEvent(input$data_file, {
    req(input$data_file)

    tryCatch({
      file_path <- input$data_file$datapath
      file_name <- input$data_file$name
      file_ext <- tolower(tools::file_ext(file_name))

      # Load data based on file extension
      data <- switch(file_ext,
                     "csv" = read.csv(file_path, stringsAsFactors = TRUE),
                     "xlsx" = {
                       df <- readxl::read_excel(file_path)
                       # Convert character columns to factors
                       df <- as.data.frame(df)
                       df[] <- lapply(df, function(x) if (is.character(x)) as.factor(x) else x)
                       df
                     },
                     "xls" = {
                       df <- readxl::read_excel(file_path)
                       df <- as.data.frame(df)
                       df[] <- lapply(df, function(x) if (is.character(x)) as.factor(x) else x)
                       df
                     },
                     "sav" = {
                       df <- haven::read_sav(file_path)
                       # Convert haven labelled columns to factors
                       df <- as.data.frame(df)
                       df[] <- lapply(df, function(x) {
                         if (haven::is.labelled(x)) {
                           as.factor(haven::as_factor(x))
                         } else if (is.character(x)) {
                           as.factor(x)
                         } else {
                           x
                         }
                       })
                       df
                     },
                     "rds" = {
                       df <- readRDS(file_path)
                       if (!is.data.frame(df)) stop("RDS file must contain a data frame")
                       df[] <- lapply(df, function(x) if (is.character(x)) as.factor(x) else x)
                       df
                     },
                     "rdata" = ,
                     "rda" = {
                       # Load into temporary environment and extract first data frame
                       temp_env <- new.env()
                       load(file_path, envir = temp_env)
                       objects <- ls(temp_env)
                       df <- NULL
                       for (obj_name in objects) {
                         obj <- get(obj_name, envir = temp_env)
                         if (is.data.frame(obj)) {
                           df <- obj
                           break
                         }
                       }
                       if (is.null(df)) stop("No data frame found in RData file")
                       df[] <- lapply(df, function(x) if (is.character(x)) as.factor(x) else x)
                       df
                     },
                     stop(paste("Unsupported file type:", file_ext))
      )

      rv$data <- data
      rv$data_dict <- NULL  # Clear dictionary when new data is loaded
      rv$model <- NULL
      rv$chat_history <- list()
      rv$chat <- NULL

      showNotification(
        paste0("Data loaded successfully! (", file_ext, " file, ", nrow(data), " rows)"),
        type = "message"
      )
    }, error = function(e) {
      showNotification(paste("Error loading data:", e$message), type = "error")
    })
  })

  # Load data dictionary
  observeEvent(input$dict_file, {
    req(input$dict_file)

    tryCatch({
      file_path <- input$dict_file$datapath
      file_name <- input$dict_file$name
      file_ext <- tolower(tools::file_ext(file_name))

      # Load dictionary based on file extension
      dict <- switch(file_ext,
                     "csv" = read.csv(file_path, stringsAsFactors = FALSE),
                     "xlsx" = as.data.frame(readxl::read_excel(file_path)),
                     "xls" = as.data.frame(readxl::read_excel(file_path)),
                     stop(paste("Unsupported dictionary file type:", file_ext))
      )

      # Standardize column names (flexible matching)
      names(dict) <- tolower(names(dict))

      # Map common column name variations
      col_mapping <- list(
        variable = c("variable", "var", "var_name", "varname", "name", "column", "col"),
        label = c("label", "short_label", "short_label_raw", "description", "desc", "var_label", "varlabel"),
        notes = c("notes", "note", "comments", "comment", "details", "detail", "context", "info")
      )

      # Find and rename columns
      final_names <- names(dict)
      for (target in names(col_mapping)) {
        for (i in seq_along(final_names)) {
          if (final_names[i] %in% col_mapping[[target]]) {
            final_names[i] <- target
            break
          }
        }
      }
      names(dict) <- final_names

      # Validate required columns
      if (!"variable" %in% names(dict)) {
        stop("Dictionary must have a 'variable' column (or: var, var_name, name, column)")
      }
      if (!"label" %in% names(dict)) {
        stop("Dictionary must have a 'label' column (or: description, desc, short_label)")
      }

      # Add notes column if missing
      if (!"notes" %in% names(dict)) {
        dict$notes <- NA_character_
      }

      # Keep only relevant columns
      dict <- dict[, c("variable", "label", "notes")]

      # Remove rows with missing variable names
      dict <- dict[!is.na(dict$variable) & dict$variable != "", ]

      rv$data_dict <- dict

      # Reset chat when dictionary changes (so AI gets new context)
      rv$chat <- NULL

      showNotification(
        paste0("Dictionary loaded! (", nrow(dict), " variable labels)"),
        type = "message"
      )
    }, error = function(e) {
      showNotification(paste("Error loading dictionary:", e$message), type = "error")
    })
  })

  # Load demo data
  observeEvent(input$use_demo, {
    if (input$use_demo) {
      # Create HCP GLP-1 Prescribing Survey dataset
      set.seed(123)
      n <- 500

      # === HCP Demographics ===
      specialty <- sample(
        c("Endocrinology", "Primary Care", "Internal Medicine", "Cardiology"),
        n, replace = TRUE,
        prob = c(0.15, 0.45, 0.25, 0.15)
      )

      years_practice <- round(runif(n, 1, 35))

      practice_setting <- sample(
        c("Academic Medical Center", "Large Group Practice", "Small Private Practice", "Health System"),
        n, replace = TRUE,
        prob = c(0.15, 0.30, 0.25, 0.30)
      )

      region <- sample(
        c("Northeast", "Southeast", "Midwest", "Southwest", "West"),
        n, replace = TRUE
      )

      # === Attitudinal Statements ===
      enthusiast_trait <- rnorm(n, 0, 1)
      barrier_trait <- rnorm(n, 0, 1)

      att_glp1_effective_a1c <- pmin(5, pmax(1, round(3.5 + 0.7 * enthusiast_trait + rnorm(n, 0, 0.8))))
      att_glp1_effective_weight <- pmin(5, pmax(1, round(3.4 + 0.8 * enthusiast_trait + rnorm(n, 0, 0.8))))
      att_glp1_cv_benefit <- pmin(5, pmax(1, round(3.0 + 0.5 * enthusiast_trait + rnorm(n, 0, 1))))

      att_gi_side_effects_concern <- pmin(5, pmax(1, round(3.0 - 0.3 * enthusiast_trait + 0.4 * barrier_trait + rnorm(n, 0, 0.9))))
      att_pancreatitis_concern <- pmin(5, pmax(1, round(2.5 + 0.5 * barrier_trait + rnorm(n, 0, 1))))
      att_patient_tolerability <- pmin(5, pmax(1, round(3.2 + 0.6 * enthusiast_trait - 0.3 * barrier_trait + rnorm(n, 0, 0.8))))

      att_cost_barrier <- pmin(5, pmax(1, round(3.3 + 0.7 * barrier_trait + rnorm(n, 0, 0.8))))
      att_pa_burden <- pmin(5, pmax(1, round(3.5 + 0.6 * barrier_trait + rnorm(n, 0, 0.9))))
      att_patient_afford <- pmin(5, pmax(1, round(2.8 - 0.6 * barrier_trait + rnorm(n, 0, 0.9))))

      att_early_intervention <- pmin(5, pmax(1, round(3.2 + 0.6 * enthusiast_trait + rnorm(n, 0, 0.9))))
      att_weight_tx_priority <- pmin(5, pmax(1, round(3.3 + 0.5 * enthusiast_trait + rnorm(n, 0, 0.9))))
      att_prefer_oral_first <- pmin(5, pmax(1, round(3.0 - 0.5 * enthusiast_trait + 0.3 * barrier_trait + rnorm(n, 0, 1))))

      att_comfortable_initiating <- pmin(5, pmax(1, round(3.0 + 0.8 * enthusiast_trait - 0.2 * barrier_trait + rnorm(n, 0, 0.8))))
      att_adequate_training <- pmin(5, pmax(1, round(3.0 + 0.5 * enthusiast_trait + rnorm(n, 0, 1))))

      att_trust_clinical_data <- pmin(5, pmax(1, round(3.5 + 0.4 * enthusiast_trait + rnorm(n, 0, 0.8))))
      att_peer_influence <- pmin(5, pmax(1, round(3.0 + rnorm(n, 0, 1))))
      att_guideline_adherence <- pmin(5, pmax(1, round(3.5 + 0.3 * enthusiast_trait + rnorm(n, 0, 0.9))))

      att_patients_interested <- pmin(5, pmax(1, round(3.2 + 0.4 * enthusiast_trait + rnorm(n, 0, 1))))
      att_patients_compliant <- pmin(5, pmax(1, round(2.9 + 0.3 * enthusiast_trait - 0.2 * barrier_trait + rnorm(n, 0, 1))))

      prescribe_score <-
        0.35 * (att_comfortable_initiating - 3) +
        0.20 * (att_glp1_effective_weight - 3) +
        0.15 * (att_glp1_effective_a1c - 3) +
        -0.25 * (att_cost_barrier - 3) +
        -0.15 * (att_prefer_oral_first - 3) +
        0.20 * (att_early_intervention - 3) +
        0.15 * (att_weight_tx_priority - 3) +
        -0.10 * (att_gi_side_effects_concern - 3) +
        0.5 * (specialty == "Endocrinology") +
        0.2 * (specialty == "Internal Medicine") +
        rnorm(n, 0, 0.5)

      threshold <- quantile(prescribe_score, 0.55)

      high_prescriber <- factor(
        ifelse(prescribe_score > threshold, "High", "Low"),
        levels = c("Low", "High")
      )

      hcp_data <- data.frame(
        High_Prescriber = high_prescriber,
        Specialty = factor(specialty),
        Years_in_Practice = years_practice,
        Practice_Setting = factor(practice_setting),
        Region = factor(region),
        Effective_for_A1C = att_glp1_effective_a1c,
        Effective_for_Weight = att_glp1_effective_weight,
        CV_Benefit_Belief = att_glp1_cv_benefit,
        GI_Side_Effect_Concern = att_gi_side_effects_concern,
        Pancreatitis_Concern = att_pancreatitis_concern,
        Patient_Tolerability = att_patient_tolerability,
        Cost_is_Barrier = att_cost_barrier,
        Prior_Auth_Burden = att_pa_burden,
        Patients_Can_Afford = att_patient_afford,
        Prefer_Early_Intervention = att_early_intervention,
        Weight_Tx_Priority = att_weight_tx_priority,
        Prefer_Oral_First = att_prefer_oral_first,
        Comfortable_Initiating = att_comfortable_initiating,
        Adequate_Training = att_adequate_training,
        Trust_Clinical_Data = att_trust_clinical_data,
        Peer_Influence = att_peer_influence,
        Follow_Guidelines = att_guideline_adherence,
        Patients_Ask_About_GLP1 = att_patients_interested,
        Patients_Compliant = att_patients_compliant
      )

      rv$data <- hcp_data
      rv$model <- NULL
      rv$chat_history <- list()
      rv$chat <- NULL

      showNotification("Demo dataset (HCP GLP-1 Attitudes Survey) loaded!", type = "message")
    }
  })

  # Update variable selectors when data changes
  observe({
    req(rv$data)

    var_names <- names(rv$data)

    updateSelectInput(
      session, "outcome_var",
      choices = var_names,
      selected = var_names[1]
    )

    updateSelectInput(
      session, "predictor_vars",
      choices = var_names,
      selected = var_names[-1]
    )
  })

  # Update AI model choices based on provider
  observeEvent(input$ai_provider, {
    if (input$ai_provider == "azure") {
      # Azure OpenAI - model comes from environment variable
      azure_model <- Sys.getenv("AZURE_OPENAI_DEPLOYMENT")
      if (azure_model == "") azure_model <- "gpt-4o"  # Default display name
      updateSelectInput(
        session, "ai_model",
        choices = setNames(azure_model, paste0("Azure: ", azure_model)),
        selected = azure_model
      )
    } else if (input$ai_provider == "ollama") {
      updateSelectInput(
        session, "ai_model",
        choices = c(
          "Ministral 3 (8B)" = "ministral-3:8b"
        ),
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
        choices = c(
          "gemini-3-flash-preview" = "gemini-3-flash-preview"
        ),
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

  # -------------------------------------------------------------------------
  # Model Building
  # -------------------------------------------------------------------------

  observeEvent(input$run_model, {
    req(rv$data, input$outcome_var, input$predictor_vars)

    # Validate predictor variables don't include outcome
    predictors <- setdiff(input$predictor_vars, input$outcome_var)

    if (length(predictors) == 0) {
      showNotification("Please select at least one predictor variable", type = "error")
      return()
    }

    withProgress(message = "Building decision tree...", {

      tryCatch({
        # Build formula
        formula_str <- paste(input$outcome_var, "~", paste(predictors, collapse = " + "))
        model_formula <- as.formula(formula_str)

        # Fit model
        rv$model <- rpart(
          formula = model_formula,
          data = rv$data,
          method = ifelse(is.factor(rv$data[[input$outcome_var]]), "class", "anova"),
          control = rpart.control(
            cp = input$cp,
            minbucket = input$minbucket,
            maxdepth = input$maxdepth
          )
        )

        # Reset chat when model changes
        rv$chat_history <- list()
        rv$chat <- NULL

        showNotification("Decision tree built successfully!", type = "message")

        # Switch to tree tab
        updateNavlistPanel(session, "main_tabs", selected = "Decision Tree")

      }, error = function(e) {
        showNotification(paste("Error building model:", e$message), type = "error")
      })
    })
  })

  # -------------------------------------------------------------------------
  # Outputs - Data
  # -------------------------------------------------------------------------

  output$data_loaded <- reactive({
    !is.null(rv$data)
  })
  outputOptions(output, "data_loaded", suspendWhenHidden = FALSE)

  output$model_built <- reactive({
    !is.null(rv$model)
  })
  outputOptions(output, "model_built", suspendWhenHidden = FALSE)

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

  # -------------------------------------------------------------------------
  # Outputs - Decision Tree
  # -------------------------------------------------------------------------

  output$tree_plot <- renderPlot({
    req(rv$model)

    rpart.plot(
      rv$model,
      type = 4,
      extra = 104,
      under = TRUE,
      fallen.leaves = TRUE,
      roundint = FALSE,
      box.palette = "BuGn",
      shadow.col = "gray",
      main = paste("Decision Tree:", input$outcome_var)
    )
  }, res = 96)

  output$var_importance <- renderPlot({
    req(rv$model)

    if (is.null(rv$model$variable.importance)) {
      return(NULL)
    }

    importance_df <- data.frame(
      Variable = names(rv$model$variable.importance),
      Importance = rv$model$variable.importance
    ) |>
      arrange(desc(Importance)) |>
      head(10) |>
      mutate(Variable = factor(Variable, levels = rev(Variable)))

    ggplot(importance_df, aes(x = Importance, y = Variable)) +
      geom_col(fill = "#18bc9c") +
      theme_minimal() +
      theme(
        axis.title.y = element_blank(),
        panel.grid.major.y = element_blank()
      ) +
      labs(x = "Importance")
  }, res = 96, height = 200)

  output$model_stats <- renderPrint({
    req(rv$model)

    cat("Formula:", deparse(rv$model$call$formula), "\n")
    cat("Method:", rv$model$method, "\n")
    cat("Number of terminal nodes:", sum(rv$model$frame$var == "<leaf>"), "\n")
    cat("Root node error:", round(rv$model$frame$dev[1] / rv$model$frame$n[1], 4), "\n")

    if (!is.null(rv$model$cptable)) {
      best_cp <- rv$model$cptable[which.min(rv$model$cptable[, "xerror"]), "CP"]
      cat("Optimal CP:", round(best_cp, 4), "\n")
    }
  })

  output$confusion_matrix <- renderDT({
    req(rv$model, rv$data, input$outcome_var)

    if (rv$model$method != "class") {
      return(NULL)
    }

    pred <- predict(rv$model, type = "class")
    actual <- rv$data[[input$outcome_var]]

    cm <- table(Predicted = pred, Actual = actual)

    datatable(
      as.data.frame.matrix(cm),
      options = list(
        dom = 't',
        paging = FALSE,
        searching = FALSE
      ),
      class = 'compact'
    )
  })

  output$tree_rules <- renderPrint({
    req(rv$model)
    rpart.rules(rv$model, style = "tall", cover = TRUE)
  })

  output$cp_table <- renderDT({
    req(rv$model)

    cp_df <- as.data.frame(rv$model$cptable)
    cp_df[] <- lapply(cp_df, function(x) round(x, 4))

    datatable(
      cp_df,
      options = list(
        dom = 't',
        paging = FALSE,
        scrollX = TRUE
      ),
      class = 'compact stripe'
    )
  })

  # -------------------------------------------------------------------------
  # AI Chat Functions
  # -------------------------------------------------------------------------

  # Helper function to format data dictionary for AI
  get_dictionary_context <- reactive({
    if (is.null(rv$data_dict)) {
      return(NULL)
    }

    dict <- rv$data_dict

    # Format dictionary entries
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

  # Helper function to get model description for AI
  get_model_context <- reactive({
    req(rv$model, rv$data, input$outcome_var)

    # Capture tree rules
    rules <- capture.output(rpart.rules(rv$model, style = "tall", cover = TRUE))

    # Variable importance - include labels if available
    var_imp <- if (!is.null(rv$model$variable.importance)) {
      var_names <- names(rv$model$variable.importance)
      var_scores <- round(rv$model$variable.importance, 2)

      # Add labels if dictionary exists
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

    # Model statistics
    n_nodes <- sum(rv$model$frame$var == "<leaf>")

    # Confusion matrix for classification
    cm_text <- ""
    if (rv$model$method == "class") {
      pred <- predict(rv$model, type = "class")
      actual <- rv$data[[input$outcome_var]]
      cm <- table(Predicted = pred, Actual = actual)

      accuracy <- sum(diag(cm)) / sum(cm)
      cm_text <- paste0(
        "\n\nConfusion Matrix:\n",
        capture.output(print(cm)) |> paste(collapse = "\n"),
        "\n\nAccuracy: ", round(accuracy * 100, 1), "%"
      )
    }

    # Get dictionary context if available
    dict_context <- get_dictionary_context()

    paste0(
      "DECISION TREE MODEL SUMMARY\n",
      "===========================\n\n",
      "Outcome Variable: ", input$outcome_var, "\n",
      "Model Type: ", ifelse(rv$model$method == "class", "Classification", "Regression"), "\n",
      "Number of Terminal Nodes: ", n_nodes, "\n",
      "Training Observations: ", nrow(rv$data), "\n",
      "\nVariable Importance:\n", var_imp, "\n",
      "\nDecision Rules:\n", paste(rules, collapse = "\n"),
      cm_text,
      "\n\nModel Parameters:\n",
      "- Complexity Parameter (cp): ", input$cp, "\n",
      "- Minimum Bucket Size: ", input$minbucket, "\n",
      "- Maximum Depth: ", input$maxdepth,
      if (!is.null(dict_context)) dict_context else ""
    )
  })

  # Initialize or get chat object
  get_chat <- function() {
    if (is.null(rv$chat)) {

      # Build system prompt with model context
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
        "explain what additional analysis might be needed."
      )

      # Create chat based on provider
      if (input$ai_provider == "azure") {
        # Azure OpenAI (secure company endpoint)
        model <- input$ai_model
        if (model == "" || is.null(model)) {
          model <- Sys.getenv("AZURE_OPENAI_DEPLOYMENT")
        }
        api_key <- Sys.getenv("AZURE_OPENAI_API_KEY")
        endpoint <- Sys.getenv("AZURE_OPENAI_ENDPOINT")

        if (any(c(api_key, endpoint, model) == "")) {
          stop("Azure environment variables not set. Please ensure AZURE_OPENAI_API_KEY, AZURE_OPENAI_ENDPOINT, and AZURE_OPENAI_DEPLOYMENT are configured.")
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
        rv$chat <- chat_ollama(
          model = input$ai_model,
          system_prompt = system_prompt
        )
      } else if (input$ai_provider == "anthropic") {
        rv$chat <- chat_anthropic(
          model = input$ai_model,
          system_prompt = system_prompt
        )
      } else if (input$ai_provider == "gemini") {
        rv$chat <- chat_google_gemini(
          model = input$ai_model,
          system_prompt = system_prompt,
          api_key = Sys.getenv("GEMINI_API_KEY")
        )
      } else {
        rv$chat <- chat_openai(
          model = input$ai_model,
          system_prompt = system_prompt
        )
      }
    }

    rv$chat
  }

  # Send message to AI
  send_to_ai <- function(message) {
    req(rv$model)

    # Add user message to history
    rv$chat_history <- c(rv$chat_history, list(
      list(role = "user", content = message)
    ))

    # Get or create chat
    chat <- get_chat()

    tryCatch({
      # Get AI response
      response <- chat$chat(message)

      # Add assistant response to history
      rv$chat_history <- c(rv$chat_history, list(
        list(role = "assistant", content = response)
      ))

    }, error = function(e) {
      error_msg <- paste("Error communicating with AI:", e$message)
      rv$chat_history <- c(rv$chat_history, list(
        list(role = "assistant", content = paste("⚠️", error_msg))
      ))
      showNotification(error_msg, type = "error")
    })
  }

  # -------------------------------------------------------------------------
  # Chat Event Handlers
  # -------------------------------------------------------------------------

  # Send user message
  observeEvent(input$send_message, {
    req(input$user_message, nzchar(trimws(input$user_message)))

    message <- trimws(input$user_message)
    updateTextAreaInput(session, "user_message", value = "")

    withProgress(message = "Thinking...", {
      send_to_ai(message)
    })
  })

  # Quick action: Interpret
  observeEvent(input$ask_interpret, {
    withProgress(message = "Analyzing...", {
      send_to_ai("Please provide a comprehensive interpretation of this decision tree. Explain the key decision paths, what variables are most important, and what the tree tells us about the outcome variable.")
    })
  })

  # Quick action: Key Insights
  observeEvent(input$ask_insights, {
    withProgress(message = "Extracting insights...", {
      send_to_ai("What are the 3-5 most important insights from this decision tree? Focus on actionable findings that would be valuable for decision-making.")
    })
  })

  # Quick action: Recommendations
  observeEvent(input$ask_recommendations, {
    withProgress(message = "Generating recommendations...", {
      send_to_ai("Based on this decision tree analysis, what recommendations would you make? Consider both the model findings and potential next steps for analysis or action.")
    })
  })

  # Quick action: Limitations
  observeEvent(input$ask_limitations, {
    withProgress(message = "Assessing limitations...", {
      send_to_ai("What are the potential limitations of this decision tree model? Consider issues like overfitting, sample size, variable selection, and generalizability.")
    })
  })

  # Clear chat
  observeEvent(input$clear_chat, {
    rv$chat_history <- list()
    rv$chat <- NULL
    showNotification("Chat cleared", type = "message")
  })

  # -------------------------------------------------------------------------
  # Chat Display
  # -------------------------------------------------------------------------

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

    # Render chat messages
    chat_items <- lapply(rv$chat_history, function(msg) {
      if (msg$role == "user") {
        div(
          class = "d-flex justify-content-end mb-3",
          div(
            class = "p-3 rounded-3",
            style = "background-color: #2c3e50; color: white; max-width: 80%;",
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
            style = "background-color: white; border: 1px solid #dee2e6; max-width: 80%;",
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

}

# ============================================================================
# Run Application
# ============================================================================

shinyApp(ui = ui, server = server)
