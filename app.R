# Dendro: AI-Powered Decision Tree Analysis
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
library(officer)
library(randomForest)
library(officer)
library(rvg)

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
    "Dendro",
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
    primary = "#6366f1",
    secondary = "#71717a",
    success = "#10b981",
    info = "#06b6d4",
    warning = "#f59e0b",
    danger = "#ef4444",
    base_font = font_google("Inter"),
    heading_font = font_google("Inter"),
    code_font = font_google("JetBrains Mono"),
    "body-bg" = "#f8f7ff",
    "card-bg" = "#ffffff",
    "border-radius" = "12px"
  ) |> bs_add_rules(sass::sass_file("www/styles.css")),

  # Suppress Shiny reconnect dialogs
  tags$head(
    tags$style(HTML("
      #shiny-disconnected-overlay, #ss-connect-dialog { display: none !important; }
    "))
  ),

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
          helpText("Select variables to include in the model"),

          # Prefix removal section
          div(
            class = "mt-2",
            style = "font-size: 0.85em;",
            div(
              class = "d-flex justify-content-between align-items-center mb-1",
              tags$label("Remove by Prefix", class = "form-label small text-muted mb-0"),
              actionButton(
                "reset_predictors",
                "Reset All",
                class = "btn-outline-secondary btn-sm",
                style = "font-size: 0.7em; padding: 1px 6px;",
                icon = icon("rotate-left")
              )
            ),
            uiOutput("prefix_buttons")
          )
        )
      ),

      accordion_panel(
        title = "Factor Levels",
        value = "factor_panel",
        icon = bsicons::bs_icon("list-ol"),

        conditionalPanel(
          condition = "!output.data_loaded",
          helpText(class = "small text-muted", "Upload data to manage factor levels")
        ),

        conditionalPanel(
          condition = "output.data_loaded",

          helpText(
            class = "small text-muted mb-2",
            "Reorder factor levels (e.g., Low < Medium < High). The first level is the reference."
          ),

          selectInput(
            "factor_var_select",
            "Factor Variable",
            choices = NULL
          ),

          uiOutput("factor_levels_display"),

          selectInput(
            "factor_level_to_move",
            NULL,
            choices = NULL,
            width = "100%"
          ),

          div(
            class = "d-flex gap-1 mt-1",
            actionButton("factor_move_up", NULL, icon = icon("chevron-up"),
                          class = "btn-sm btn-outline-secondary flex-fill"),
            actionButton("factor_move_down", NULL, icon = icon("chevron-down"),
                          class = "btn-sm btn-outline-secondary flex-fill"),
            actionButton("factor_set_ref", "Set as First",
                          class = "btn-sm btn-outline-secondary flex-fill")
          ),

          actionButton("factor_apply", "Apply Order",
                        class = "btn-primary btn-sm w-100 mt-2",
                        icon = icon("check"))
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
              selected = "openai"
            )
          )
        },

        selectInput(
          "ai_model",
          "Model",
          choices = NULL,
          selected = NULL
        ),

        selectInput(
          "user_persona",
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

    # Data Quality Tab
    nav_panel(
      title = "Data Quality",
      icon = bsicons::bs_icon("clipboard-check"),

      conditionalPanel(
        condition = "!output.data_loaded",
        card(
          card_body(
            div(
              class = "text-center py-5",
              bsicons::bs_icon("clipboard-check", size = "4rem", class = "text-muted"),
              h4(class = "text-muted mt-3", "Upload data to see quality checks"),
              p(class = "text-muted", "Data validation will appear here after you upload a dataset")
            )
          )
        )
      ),

      conditionalPanel(
        condition = "output.data_loaded",

        layout_columns(
          col_widths = c(12),

          # Summary card
          card(
            card_body(
              class = "p-3",
              layout_columns(
                col_widths = c(3, 3, 3, 3),
                value_box(
                  title = "Ready for Analysis",
                  value = textOutput("dq_ready_vars", inline = TRUE),
                  showcase = bsicons::bs_icon("check-circle"),
                  showcase_layout = "left center",
                  theme = "success"
                ),
                value_box(
                  title = "Warnings",
                  value = textOutput("dq_warning_vars", inline = TRUE),
                  showcase = bsicons::bs_icon("exclamation-triangle"),
                  showcase_layout = "left center",
                  theme = "warning"
                ),
                value_box(
                  title = "Issues",
                  value = textOutput("dq_issue_vars", inline = TRUE),
                  showcase = bsicons::bs_icon("x-circle"),
                  showcase_layout = "left center",
                  theme = "danger"
                ),
                value_box(
                  title = "Overall Status",
                  value = textOutput("dq_overall_status", inline = TRUE),
                  showcase = bsicons::bs_icon("speedometer2"),
                  showcase_layout = "left center",
                  theme = "info"
                )
              )
            )
          )
        ),

        layout_columns(
          col_widths = c(6, 6),

          # Variable Quality Table
          card(
            card_header("Variable Quality Summary"),
            card_body(
              p(class = "text-muted small", "Click a row to see details. Variables are flagged based on missingness, variance, and data type."),
              DTOutput("dq_variable_table")
            )
          ),

          # Issue Details
          card(
            card_header("Quality Check Details"),
            card_body(
              accordion(
                id = "dq_accordion",
                open = FALSE,

                accordion_panel(
                  title = "Missing Data",
                  value = "missing",
                  icon = bsicons::bs_icon("question-circle"),
                  p(class = "small text-muted", "Variables with missing values. High missingness (>50%) may indicate skip logic or data collection issues."),
                  DTOutput("dq_missing_table")
                ),

                accordion_panel(
                  title = "Low Variance",
                  value = "variance",
                  icon = bsicons::bs_icon("dash-circle"),
                  p(class = "small text-muted", "Variables with very low variance won't contribute meaningful splits. Consider excluding near-constant variables."),
                  DTOutput("dq_variance_table")
                ),

                accordion_panel(
                  title = "Outcome Variable Check",
                  value = "outcome",
                  icon = bsicons::bs_icon("bullseye"),
                  p(class = "small text-muted", "Assessment of your selected outcome variable for decision tree suitability."),
                  uiOutput("dq_outcome_check")
                ),

                accordion_panel(
                  title = "Sample Size",
                  value = "sample",
                  icon = bsicons::bs_icon("people"),
                  p(class = "small text-muted", "Minimum recommended sample sizes for reliable decision tree analysis."),
                  uiOutput("dq_sample_check")
                ),

                accordion_panel(
                  title = "High Correlation",
                  value = "correlation",
                  icon = bsicons::bs_icon("link-45deg"),
                  p(class = "small text-muted", "Highly correlated predictors. Common in attitudinal data - not necessarily a problem, but good to be aware of."),
                  DTOutput("dq_correlation_table")
                )
              )
            )
          )
        ),

        # Recommendations
        layout_columns(
          col_widths = c(12),
          card(
            card_header(
              class = "d-flex align-items-center",
              bsicons::bs_icon("lightbulb", class = "me-2"),
              "Recommendations"
            ),
            card_body(
              uiOutput("dq_recommendations")
            )
          )
        )
      )
    ),

    # Decision Tree Tab (with AI Chat)
    nav_panel(
      title = "Decision Tree",
      icon = bsicons::bs_icon("diagram-3"),

      # Toggle button for AI panel
      div(
        class = "mb-2 d-flex justify-content-end",
        actionButton(
          "toggle_ai_panel",
          "Show AI Chat",
          class = "btn-sm btn-outline-secondary",
          icon = icon("robot")
        )
      ),

      uiOutput("tree_layout")
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
            card_header(
              class = "d-flex align-items-center",
              "Random Forest Importance",
              tags$span(
                class = "ms-2 badge bg-secondary",
                style = "font-weight: normal; font-size: 0.7em;",
                "Validation"
              )
            ),
            card_body(
              conditionalPanel(
                condition = "output.model_built",
                p(class = "text-muted small", "Random Forest provides a complementary importance measure based on 500 trees. Variables important in both methods are more reliably predictive."),
                plotOutput("rf_importance", height = "280px")
              ),
              conditionalPanel(
                condition = "!output.model_built",
                div(class = "text-center py-3 text-muted", "Build a model to see Random Forest importance")
              )
            )
          ),

          card(
            card_header("Importance Comparison"),
            card_body(
              conditionalPanel(
                condition = "output.model_built",
                p(class = "text-muted small", "Side-by-side comparison of variable rankings from both methods."),
                DTOutput("importance_comparison")
              ),
              conditionalPanel(
                condition = "!output.model_built",
                div(class = "text-center py-3 text-muted", "Build a model to compare importance")
              )
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
            h4("Dendro User Guide"),
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
    chat = NULL,  # ellmer chat object
    show_ai_panel = FALSE,  # AI chat panel visibility
    factor_levels_pending = list()  # pending factor level reorders
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
      rv$factor_levels_pending <- list()

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
      rv$factor_levels_pending <- list()

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

  # -------------------------------------------------------------------------
  # Prefix-based Variable Removal
  # -------------------------------------------------------------------------

  # Generate prefix removal buttons
  output$prefix_buttons <- renderUI({
    req(input$predictor_vars)

    # Extract prefixes (text before underscore or first 3+ chars if no underscore)
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

    # Count variables per prefix (only show prefixes with 2+ variables)
    prefix_counts <- table(prefixes)
    multi_prefixes <- names(prefix_counts[prefix_counts >= 2])

    if (length(multi_prefixes) == 0) {
      return(helpText(class = "small text-muted", "No common prefixes detected"))
    }

    # Sort by count descending
    multi_prefixes <- multi_prefixes[order(-prefix_counts[multi_prefixes])]

    # Create buttons for each prefix
    buttons <- lapply(multi_prefixes, function(prefix) {
      count <- prefix_counts[prefix]
      actionButton(
        inputId = paste0("remove_prefix_", prefix),
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

    # Extract all possible prefixes
    prefixes <- unique(sapply(vars, function(v) {
      if (grepl("_", v)) sub("_.*", "", v) else if (nchar(v) >= 3) substr(v, 1, 3) else v
    }))

    # Create observers for each prefix button
    lapply(prefixes, function(prefix) {
      observeEvent(input[[paste0("remove_prefix_", prefix)]], {
        current_vars <- input$predictor_vars

        # Find variables that start with this prefix
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
    # Select all variables except the current outcome variable
    outcome <- input$outcome_var
    predictors <- setdiff(all_vars, outcome)
    updateSelectInput(session, "predictor_vars", selected = predictors)
    showNotification("Predictor variables reset to full list", type = "message")
  })

  # -------------------------------------------------------------------------
  # Factor Level Management
  # -------------------------------------------------------------------------

  # Update factor variable selector when data changes
  observe({
    req(rv$data)
    factor_vars <- names(rv$data)[sapply(rv$data, is.factor)]
    if (length(factor_vars) == 0) {
      updateSelectInput(session, "factor_var_select",
                        choices = c("No factor variables" = ""),
                        selected = "")
    } else {
      updateSelectInput(session, "factor_var_select",
                        choices = factor_vars,
                        selected = factor_vars[1])
    }
  })

  # Get current levels for selected factor (pending or actual)
  current_factor_levels <- reactive({
    req(input$factor_var_select, rv$data)
    var <- input$factor_var_select
    if (var == "" || !var %in% names(rv$data)) return(NULL)
    if (!is.factor(rv$data[[var]])) return(NULL)

    pending <- rv$factor_levels_pending
    if (var %in% names(pending)) {
      pending[[var]]
    } else {
      levels(rv$data[[var]])
    }
  })

  # Display current levels as numbered list
  output$factor_levels_display <- renderUI({
    lvls <- current_factor_levels()
    if (is.null(lvls)) {
      return(helpText(class = "small text-muted", "Select a factor variable above"))
    }

    level_items <- lapply(seq_along(lvls), function(i) {
      div(
        class = "d-flex align-items-center py-1 px-2",
        style = if (i == 1) "font-weight: 600;" else "",
        tags$small(class = "text-muted me-2", style = "min-width: 20px;", paste0(i, ".")),
        span(style = "font-size: 0.85em;", lvls[i]),
        if (i == 1) tags$small(class = "ms-auto text-muted fst-italic", "ref") else NULL
      )
    })

    div(
      class = "border rounded mb-2",
      style = "max-height: 200px; overflow-y: auto;",
      tagList(level_items)
    )
  })

  # Update the level-to-move selector
  observe({
    lvls <- current_factor_levels()
    if (is.null(lvls)) return()
    selected <- input$factor_level_to_move
    if (is.null(selected) || !selected %in% lvls) selected <- lvls[1]
    updateSelectInput(session, "factor_level_to_move", choices = lvls, selected = selected)
  })

  # Move level up
  observeEvent(input$factor_move_up, {
    req(input$factor_var_select, input$factor_level_to_move)
    var <- input$factor_var_select
    lvls <- current_factor_levels()
    if (is.null(lvls)) return()

    idx <- which(lvls == input$factor_level_to_move)
    if (length(idx) == 0 || idx <= 1) return()

    lvls[c(idx - 1, idx)] <- lvls[c(idx, idx - 1)]
    pending <- rv$factor_levels_pending
    pending[[var]] <- lvls
    rv$factor_levels_pending <- pending
  })

  # Move level down
  observeEvent(input$factor_move_down, {
    req(input$factor_var_select, input$factor_level_to_move)
    var <- input$factor_var_select
    lvls <- current_factor_levels()
    if (is.null(lvls)) return()

    idx <- which(lvls == input$factor_level_to_move)
    if (length(idx) == 0 || idx >= length(lvls)) return()

    lvls[c(idx, idx + 1)] <- lvls[c(idx + 1, idx)]
    pending <- rv$factor_levels_pending
    pending[[var]] <- lvls
    rv$factor_levels_pending <- pending
  })

  # Set as reference (move to position 1)
  observeEvent(input$factor_set_ref, {
    req(input$factor_var_select, input$factor_level_to_move)
    var <- input$factor_var_select
    lvls <- current_factor_levels()
    if (is.null(lvls)) return()

    selected <- input$factor_level_to_move
    lvls <- c(selected, setdiff(lvls, selected))
    pending <- rv$factor_levels_pending
    pending[[var]] <- lvls
    rv$factor_levels_pending <- pending
  })

  # Apply factor level reorder
  observeEvent(input$factor_apply, {
    req(input$factor_var_select, rv$data)
    var <- input$factor_var_select

    if (!var %in% names(rv$factor_levels_pending)) {
      showNotification("No changes to apply", type = "message")
      return()
    }

    new_levels <- rv$factor_levels_pending[[var]]
    rv$data[[var]] <- factor(rv$data[[var]], levels = new_levels)

    # Clear pending for this variable
    pending <- rv$factor_levels_pending
    pending[[var]] <- NULL
    rv$factor_levels_pending <- pending

    # Reset model since data changed
    rv$model <- NULL
    rv$chat <- NULL
    rv$chat_history <- list()

    showNotification(
      paste0("Factor levels reordered for '", var, "'"),
      type = "message"
    )
  })

  # -------------------------------------------------------------------------
  # AI Panel Toggle
  # -------------------------------------------------------------------------

  observeEvent(input$toggle_ai_panel, {
    rv$show_ai_panel <- !rv$show_ai_panel
    # Update button text
    updateActionButton(
      session, "toggle_ai_panel",
      label = if (rv$show_ai_panel) "Hide AI Chat" else "Show AI Chat"
    )
  })

  # Dynamic tree layout based on AI panel visibility
  output$tree_layout <- renderUI({
    if (rv$show_ai_panel) {
      # Two-column layout with AI chat
      layout_columns(
        col_widths = c(7, 5),
        # Tree visualization card
        tree_card_ui(),
        # AI chat card
        ai_chat_card_ui()
      )
    } else {
      # Full-width tree only
      tree_card_ui()
    }
  })

  # Helper function for tree card UI
  tree_card_ui <- function() {
    card(
      card_header(
        class = "d-flex justify-content-between align-items-center",
        span("Decision Tree Visualization"),
        conditionalPanel(
          condition = "output.model_built",
          downloadButton(
            "export_pptx",
            "Export",
            class = "btn-sm btn-outline-primary",
            icon = icon("file-powerpoint")
          )
        )
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
          plotOutput("tree_plot", height = if (rv$show_ai_panel) "550px" else "650px")
        )
      )
    )
  }

  # Helper function for AI chat card UI
  ai_chat_card_ui <- function() {
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

          # Chat history display
          div(
            id = "chat_container",
            class = "flex-grow-1",
            style = "overflow-y: auto; border: 1px solid #e8e5f5; border-radius: 12px; padding: 15px; background: linear-gradient(180deg, #f1f0fb, #f8f7ff); min-height: 0;",
            uiOutput("chat_history")
          ),

          # Quick action buttons
          div(
            class = "mt-2 mb-2 flex-shrink-0",
            actionButton("ask_interpret", "Interpret", class = "btn-outline-primary btn-sm me-1", icon = icon("lightbulb")),
            actionButton("ask_insights", "Insights", class = "btn-outline-primary btn-sm me-1", icon = icon("chart-line")),
            actionButton("ask_recommendations", "Recommend", class = "btn-outline-primary btn-sm me-1", icon = icon("list-check")),
            actionButton("ask_limitations", "Limits", class = "btn-outline-primary btn-sm", icon = icon("exclamation-triangle"))
          ),

          # User input
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

  # Update AI model choices based on provider
  observeEvent(input$ai_provider, {
    # Reset chat when provider changes
    rv$chat <- NULL

    if (input$ai_provider == "azure") {
      # Azure OpenAI - model comes from environment variable
      azure_model <- Sys.getenv("AZURE_OPENAI_DEPLOYMENT")
      if (azure_model == "") azure_model <- "gpt-5-mini"  # Default display name
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

  # Reset chat when persona changes so new system prompt takes effect
  observeEvent(input$user_persona, {
    rv$chat <- NULL
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

        # Create a copy of the data for modeling
        model_data <- rv$data

        # Convert binary numeric variables (0/1) to factors for cleaner splits
        # This prevents splits like "< 0.5" and instead treats them as categorical
        for (var in c(input$outcome_var, predictors)) {
          if (var %in% names(model_data)) {
            col <- model_data[[var]]
            # Check if numeric and binary (only 0 and 1 values, possibly with NA)
            if (is.numeric(col) && !is.factor(col)) {
              unique_vals <- unique(na.omit(col))
              if (length(unique_vals) == 2 && all(unique_vals %in% c(0, 1))) {
                model_data[[var]] <- factor(col, levels = c(0, 1), labels = c("0", "1"))
              }
            }
          }
        }

        # Fit model
        rv$model <- rpart(
          formula = model_formula,
          data = model_data,
          method = ifelse(is.factor(model_data[[input$outcome_var]]), "class", "anova"),
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
  # Outputs - Data Quality
  # -------------------------------------------------------------------------

  # Reactive to compute all data quality metrics
  data_quality <- reactive({
    req(rv$data)

    df <- rv$data
    n_obs <- nrow(df)
    n_vars <- ncol(df)

    # --- Missing Data Analysis ---
    missing_info <- data.frame(
      Variable = names(df),
      N_Missing = sapply(df, function(x) sum(is.na(x))),
      Pct_Missing = sapply(df, function(x) round(sum(is.na(x)) / length(x) * 100, 1)),
      stringsAsFactors = FALSE
    )
    missing_info$Status <- case_when(
      missing_info$Pct_Missing == 0 ~ "OK",
      missing_info$Pct_Missing < 20 ~ "Low",
      missing_info$Pct_Missing < 50 ~ "Moderate",
      TRUE ~ "High"
    )

    # --- Variance Analysis ---
    variance_info <- data.frame(
      Variable = names(df),
      stringsAsFactors = FALSE
    )

    variance_info$Type <- sapply(df, function(x) {
      if (is.factor(x) || is.character(x)) "Categorical"
      else if (is.numeric(x)) "Numeric"
      else "Other"
    })

    variance_info$Unique_Values <- sapply(df, function(x) length(unique(na.omit(x))))

    variance_info$Variance_Status <- sapply(seq_len(ncol(df)), function(i) {
      x <- df[[i]]
      x <- na.omit(x)
      if (length(x) == 0) return("No Data")

      n_unique <- length(unique(x))

      if (n_unique == 1) return("Constant")

      if (is.numeric(x)) {
        # Check if >95% of values are the same
        most_common_pct <- max(table(x)) / length(x)
        if (most_common_pct > 0.95) return("Near-Constant")
      } else {
        # For categorical: check if one level dominates
        most_common_pct <- max(table(x)) / length(x)
        if (most_common_pct > 0.95) return("Near-Constant")
      }

      return("OK")
    })

    # --- Correlation Analysis (numeric variables only) ---
    numeric_vars <- names(df)[sapply(df, is.numeric)]
    high_corr <- data.frame(Var1 = character(), Var2 = character(), Correlation = numeric(), stringsAsFactors = FALSE)

    if (length(numeric_vars) >= 2) {
      numeric_df <- df[, numeric_vars, drop = FALSE]
      # Use pairwise complete observations
      cor_matrix <- tryCatch({
        cor(numeric_df, use = "pairwise.complete.obs")
      }, error = function(e) NULL)

      if (!is.null(cor_matrix)) {
        for (i in 1:(ncol(cor_matrix) - 1)) {
          for (j in (i + 1):ncol(cor_matrix)) {
            if (!is.na(cor_matrix[i, j]) && abs(cor_matrix[i, j]) > 0.8) {
              high_corr <- rbind(high_corr, data.frame(
                Var1 = numeric_vars[i],
                Var2 = numeric_vars[j],
                Correlation = round(cor_matrix[i, j], 3),
                stringsAsFactors = FALSE
              ))
            }
          }
        }
      }
    }

    # --- Variable Summary ---
    var_summary <- merge(missing_info, variance_info, by = "Variable")
    var_summary$Overall_Status <- case_when(
      var_summary$Variance_Status == "Constant" ~ "Issue",
      var_summary$Variance_Status == "No Data" ~ "Issue",
      var_summary$Pct_Missing > 50 ~ "Warning",
      var_summary$Variance_Status == "Near-Constant" ~ "Warning",
      var_summary$Pct_Missing > 20 ~ "Warning",
      TRUE ~ "OK"
    )

    # Add labels from dictionary if available
    if (!is.null(rv$data_dict)) {
      var_summary$Label <- sapply(var_summary$Variable, function(v) {
        match_idx <- match(v, rv$data_dict$variable)
        if (!is.na(match_idx)) rv$data_dict$label[match_idx] else ""
      })
    } else {
      var_summary$Label <- ""
    }

    # --- Counts ---
    n_ok <- sum(var_summary$Overall_Status == "OK")
    n_warning <- sum(var_summary$Overall_Status == "Warning")
    n_issue <- sum(var_summary$Overall_Status == "Issue")

    list(
      missing = missing_info,
      variance = variance_info,
      correlation = high_corr,
      summary = var_summary,
      n_ok = n_ok,
      n_warning = n_warning,
      n_issue = n_issue,
      n_obs = n_obs,
      n_vars = n_vars
    )
  })

  # Summary value boxes
  output$dq_ready_vars <- renderText({
    req(data_quality())
    data_quality()$n_ok
  })

  output$dq_warning_vars <- renderText({
    req(data_quality())
    data_quality()$n_warning
  })

  output$dq_issue_vars <- renderText({
    req(data_quality())
    data_quality()$n_issue
  })

  output$dq_overall_status <- renderText({
    req(data_quality())
    dq <- data_quality()
    if (dq$n_issue > 0) {
      "Review Needed"
    } else if (dq$n_warning > 3) {
      "Some Concerns"
    } else {
      "Good"
    }
  })

  # Variable quality table
  output$dq_variable_table <- renderDT({
    req(data_quality())

    df <- data_quality()$summary
    df <- df[, c("Variable", "Label", "Type", "Pct_Missing", "Unique_Values", "Overall_Status")]
    names(df) <- c("Variable", "Label", "Type", "% Missing", "Unique", "Status")

    datatable(
      df,
      options = list(
        pageLength = 15,
        dom = 'frtip',
        scrollX = TRUE,
        order = list(list(5, 'desc'))  # Sort by status
      ),
      class = 'compact stripe hover',
      selection = 'single'
    ) |>
      formatStyle(
        'Status',
        backgroundColor = styleEqual(
          c('OK', 'Warning', 'Issue'),
          c('#d1fae5', '#fef3c7', '#fee2e2')
        ),
        fontWeight = 'bold'
      )
  })

  # Missing data table
  output$dq_missing_table <- renderDT({
    req(data_quality())

    df <- data_quality()$missing
    df <- df[df$Pct_Missing > 0, ]
    df <- df[order(-df$Pct_Missing), ]

    if (nrow(df) == 0) {
      return(datatable(data.frame(Message = "No missing data found"), options = list(dom = 't')))
    }

    names(df) <- c("Variable", "N Missing", "% Missing", "Status")

    datatable(
      df,
      options = list(pageLength = 10, dom = 'tip'),
      class = 'compact stripe'
    ) |>
      formatStyle(
        'Status',
        backgroundColor = styleEqual(
          c('OK', 'Low', 'Moderate', 'High'),
          c('#d1fae5', '#d1fae5', '#fef3c7', '#fee2e2')
        )
      )
  })

  # Low variance table
  output$dq_variance_table <- renderDT({
    req(data_quality())

    df <- data_quality()$variance
    df <- df[df$Variance_Status != "OK", ]

    if (nrow(df) == 0) {
      return(datatable(data.frame(Message = "All variables have adequate variance"), options = list(dom = 't')))
    }

    names(df) <- c("Variable", "Type", "Unique Values", "Status")

    datatable(
      df,
      options = list(pageLength = 10, dom = 'tip'),
      class = 'compact stripe'
    ) |>
      formatStyle(
        'Status',
        backgroundColor = styleEqual(
          c('OK', 'Near-Constant', 'Constant', 'No Data'),
          c('#d1fae5', '#fef3c7', '#fee2e2', '#fee2e2')
        )
      )
  })

  # Correlation table
  output$dq_correlation_table <- renderDT({
    req(data_quality())

    df <- data_quality()$correlation

    if (nrow(df) == 0) {
      return(datatable(data.frame(Message = "No highly correlated pairs found (r > 0.8)"), options = list(dom = 't')))
    }

    df <- df[order(-abs(df$Correlation)), ]

    datatable(
      df,
      options = list(pageLength = 10, dom = 'tip'),
      class = 'compact stripe'
    )
  })

  # Outcome variable check
  output$dq_outcome_check <- renderUI({
    req(rv$data, input$outcome_var)

    outcome <- rv$data[[input$outcome_var]]
    n_obs <- length(outcome)
    n_missing <- sum(is.na(outcome))
    n_complete <- n_obs - n_missing

    checks <- list()

    if (is.factor(outcome) || is.character(outcome)) {
      # Classification
      levels_tbl <- table(outcome)
      n_levels <- length(levels_tbl)
      min_level_n <- min(levels_tbl)
      min_level_pct <- round(min(levels_tbl) / sum(levels_tbl) * 100, 1)

      checks$type <- list(
        status = "OK",
        text = paste0("Classification outcome with ", n_levels, " levels")
      )

      if (n_levels < 2) {
        checks$levels <- list(status = "Issue", text = "Only 1 level - cannot build classifier")
      } else if (n_levels > 10) {
        checks$levels <- list(status = "Warning", text = paste0(n_levels, " levels - consider grouping for interpretability"))
      } else {
        checks$levels <- list(status = "OK", text = paste0(n_levels, " levels"))
      }

      if (min_level_pct < 5) {
        checks$balance <- list(status = "Warning", text = paste0("Smallest class has only ", min_level_pct, "% of cases (n=", min_level_n, ")"))
      } else if (min_level_pct < 10) {
        checks$balance <- list(status = "OK", text = paste0("Minor imbalance - smallest class: ", min_level_pct, "%"))
      } else {
        checks$balance <- list(status = "OK", text = paste0("Balanced classes - smallest: ", min_level_pct, "%"))
      }

    } else if (is.numeric(outcome)) {
      # Regression
      checks$type <- list(status = "OK", text = "Regression outcome (continuous)")

      n_unique <- length(unique(na.omit(outcome)))
      if (n_unique < 5) {
        checks$levels <- list(status = "Warning", text = paste0("Only ", n_unique, " unique values - consider treating as categorical"))
      } else {
        checks$levels <- list(status = "OK", text = paste0(n_unique, " unique values"))
      }

      checks$balance <- list(status = "OK", text = "N/A for regression")
    }

    # Missing check
    if (n_missing > 0) {
      pct_missing <- round(n_missing / n_obs * 100, 1)
      if (pct_missing > 20) {
        checks$missing <- list(status = "Warning", text = paste0(pct_missing, "% missing values in outcome"))
      } else {
        checks$missing <- list(status = "OK", text = paste0(pct_missing, "% missing (will be excluded)"))
      }
    } else {
      checks$missing <- list(status = "OK", text = "No missing values")
    }

    # Build UI
    check_items <- lapply(names(checks), function(nm) {
      check <- checks[[nm]]
      icon_name <- switch(check$status,
                          "OK" = "check-circle",
                          "Warning" = "exclamation-triangle",
                          "Issue" = "x-circle"
      )
      icon_color <- switch(check$status,
                           "OK" = "text-success",
                           "Warning" = "text-warning",
                           "Issue" = "text-danger"
      )
      div(
        class = "d-flex align-items-center mb-2",
        bsicons::bs_icon(icon_name, class = paste("me-2", icon_color)),
        span(check$text)
      )
    })

    div(
      h6(paste0("Outcome: ", input$outcome_var)),
      tagList(check_items)
    )
  })

  # Sample size check
  output$dq_sample_check <- renderUI({
    req(rv$data, input$outcome_var, input$predictor_vars)

    n_obs <- nrow(rv$data)
    n_predictors <- length(input$predictor_vars)
    outcome <- rv$data[[input$outcome_var]]

    checks <- list()

    # Overall sample size
    if (n_obs < 100) {
      checks$overall <- list(status = "Warning", text = paste0("Small sample (n=", n_obs, ") - tree may be unstable"))
    } else if (n_obs < 200) {
      checks$overall <- list(status = "OK", text = paste0("Adequate sample (n=", n_obs, ")"))
    } else {
      checks$overall <- list(status = "OK", text = paste0("Good sample size (n=", n_obs, ")"))
    }

    # Per outcome level (for classification)
    if (is.factor(outcome) || is.character(outcome)) {
      min_level_n <- min(table(outcome))
      if (min_level_n < 30) {
        checks$per_level <- list(status = "Warning", text = paste0("Smallest outcome group has only ", min_level_n, " cases (recommend 30+)"))
      } else if (min_level_n < 50) {
        checks$per_level <- list(status = "OK", text = paste0("Smallest outcome group: ", min_level_n, " cases"))
      } else {
        checks$per_level <- list(status = "OK", text = paste0("Good outcome group sizes (min: ", min_level_n, ")"))
      }
    }

    # Observations per predictor
    obs_per_pred <- round(n_obs / max(n_predictors, 1))
    if (obs_per_pred < 10) {
      checks$ratio <- list(status = "Warning", text = paste0("Only ", obs_per_pred, " observations per predictor (recommend 10+)"))
    } else {
      checks$ratio <- list(status = "OK", text = paste0(obs_per_pred, " observations per predictor"))
    }

    # Build UI
    check_items <- lapply(names(checks), function(nm) {
      check <- checks[[nm]]
      icon_name <- switch(check$status,
                          "OK" = "check-circle",
                          "Warning" = "exclamation-triangle",
                          "Issue" = "x-circle"
      )
      icon_color <- switch(check$status,
                           "OK" = "text-success",
                           "Warning" = "text-warning",
                           "Issue" = "text-danger"
      )
      div(
        class = "d-flex align-items-center mb-2",
        bsicons::bs_icon(icon_name, class = paste("me-2", icon_color)),
        span(check$text)
      )
    })

    div(tagList(check_items))
  })

  # Recommendations
  output$dq_recommendations <- renderUI({
    req(data_quality())
    dq <- data_quality()

    recommendations <- list()

    # Check for constant variables
    constant_vars <- dq$variance$Variable[dq$variance$Variance_Status == "Constant"]
    if (length(constant_vars) > 0) {
      recommendations <- c(recommendations, list(
        div(
          class = "d-flex mb-2",
          bsicons::bs_icon("x-circle", class = "text-danger me-2 flex-shrink-0"),
          span(
            tags$strong("Remove constant variables: "),
            paste(constant_vars, collapse = ", "),
            " - these have only one value and cannot contribute to the model."
          )
        )
      ))
    }

    # Check for high missing
    high_missing <- dq$missing$Variable[dq$missing$Pct_Missing > 50]
    if (length(high_missing) > 0) {
      recommendations <- c(recommendations, list(
        div(
          class = "d-flex mb-2",
          bsicons::bs_icon("exclamation-triangle", class = "text-warning me-2 flex-shrink-0"),
          span(
            tags$strong("Review high-missingness variables: "),
            paste(high_missing, collapse = ", "),
            " - >50% missing. This may be due to skip logic (OK) or data issues (investigate)."
          )
        )
      ))
    }

    # Check for many correlated pairs
    if (nrow(dq$correlation) > 5) {
      recommendations <- c(recommendations, list(
        div(
          class = "d-flex mb-2",
          bsicons::bs_icon("info-circle", class = "text-info me-2 flex-shrink-0"),
          span(
            tags$strong("Multiple correlated predictors detected. "),
            "This is common in attitudinal surveys. The tree will select the most predictive variable at each split, but be aware that similar variables may be interchangeable."
          )
        )
      ))
    }

    # Sample size
    if (dq$n_obs < 100) {
      recommendations <- c(recommendations, list(
        div(
          class = "d-flex mb-2",
          bsicons::bs_icon("exclamation-triangle", class = "text-warning me-2 flex-shrink-0"),
          span(
            tags$strong("Small sample size (n=", dq$n_obs, "). "),
            "Consider using higher cp values (0.02-0.05) to prevent overfitting and ensure stable results."
          )
        )
      ))
    }

    if (length(recommendations) == 0) {
      recommendations <- list(
        div(
          class = "d-flex mb-2",
          bsicons::bs_icon("check-circle", class = "text-success me-2 flex-shrink-0"),
          span(tags$strong("Data looks good! "), "No major issues detected. You're ready to build your decision tree.")
        )
      )
    }

    tagList(recommendations)
  })

  # -------------------------------------------------------------------------
  # Outputs - Decision Tree
  # -------------------------------------------------------------------------

  output$tree_plot <- renderPlot({
    req(rv$model)

    # Use extra=104 for classification (shows class probabilities + %)
    # Use extra=101 for regression/anova (shows n + %)
    extra_val <- if (rv$model$method == "class") 104 else 101

    rpart.plot(
      rv$model,
      type = 4,
      extra = extra_val,
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
      geom_col(fill = "#6366f1") +
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

  # Random Forest importance
  rf_model <- reactive({
    req(rv$model, rv$data, input$outcome_var, input$predictor_vars)

    # Prepare data - remove rows with any NA in outcome or predictors
    predictors <- setdiff(input$predictor_vars, input$outcome_var)
    model_data <- rv$data[, c(input$outcome_var, predictors), drop = FALSE]
    model_data <- model_data[complete.cases(model_data), ]

    # Need at least 50 rows for RF
    if (nrow(model_data) < 50) return(NULL)

    # Build formula
    formula_str <- paste(input$outcome_var, "~", paste(predictors, collapse = " + "))
    model_formula <- as.formula(formula_str)

    # Run Random Forest with error handling
    tryCatch({
      withProgress(message = "Computing Random Forest importance...", value = 0.5, {
        rf <- randomForest(
          formula = model_formula,
          data = model_data,
          ntree = 500,
          importance = TRUE,
          na.action = na.omit
        )
        rf
      })
    }, error = function(e) {
      NULL
    })
  })

  output$rf_importance <- renderPlot({
    rf <- rf_model()

    # Handle case where RF couldn't be computed
    if (is.null(rf)) {
      # Return a message plot
      ggplot() +
        annotate("text", x = 0.5, y = 0.5,
                 label = "Random Forest could not be computed.\n\nPossible reasons:\n• Fewer than 50 complete cases\n• Data type incompatibility\n• Too many factor levels",
                 size = 4, color = "#86868b", hjust = 0.5, vjust = 0.5) +
        theme_void() +
        xlim(0, 1) + ylim(0, 1)
    } else {
      # Get importance - use MeanDecreaseGini for classification, %IncMSE for regression
      if (rf$type == "classification") {
        imp <- importance(rf, type = 2)  # MeanDecreaseGini
        imp_df <- data.frame(
          Variable = rownames(imp),
          Importance = imp[, 1]
        )
      } else {
        imp <- importance(rf, type = 1)  # %IncMSE
        imp_df <- data.frame(
          Variable = rownames(imp),
          Importance = imp[, 1]
        )
      }

      imp_df <- imp_df |>
        arrange(desc(Importance)) |>
        head(10) |>
        mutate(Variable = factor(Variable, levels = rev(Variable)))

      # Add labels from dictionary if available
      if (!is.null(rv$data_dict)) {
        imp_df$Label <- sapply(as.character(imp_df$Variable), function(v) {
          match_idx <- match(v, rv$data_dict$variable)
          if (!is.na(match_idx)) rv$data_dict$label[match_idx] else v
        })
        imp_df$DisplayName <- ifelse(nchar(imp_df$Label) > 0, imp_df$Label, as.character(imp_df$Variable))
        imp_df$DisplayName <- factor(imp_df$DisplayName, levels = rev(imp_df$DisplayName))
      } else {
        imp_df$DisplayName <- imp_df$Variable
      }

      ggplot(imp_df, aes(x = Importance, y = DisplayName)) +
        geom_col(fill = "#8b5cf6") +
        theme_minimal(base_size = 12) +
        theme(
          axis.title.y = element_blank(),
          panel.grid.major.y = element_blank()
        ) +
        labs(x = "Importance (Mean Decrease Gini)")
    }
  }, res = 96)

  output$importance_comparison <- renderDT({
    req(rv$model)

    rf <- rf_model()

    # If RF failed, show only decision tree importance
    if (is.null(rf)) {
      if (!is.null(rv$model$variable.importance)) {
        dt_imp <- data.frame(
          Variable = names(rv$model$variable.importance),
          DT_Importance = round(rv$model$variable.importance, 2),
          stringsAsFactors = FALSE
        )
        dt_imp$DT_Rank <- rank(-dt_imp$DT_Importance, ties.method = "min")
        dt_imp <- dt_imp[order(dt_imp$DT_Rank), ]

        # Add labels from dictionary
        if (!is.null(rv$data_dict)) {
          dt_imp$Label <- sapply(dt_imp$Variable, function(v) {
            match_idx <- match(v, rv$data_dict$variable)
            if (!is.na(match_idx)) rv$data_dict$label[match_idx] else ""
          })
          dt_imp <- dt_imp[, c("Variable", "Label", "DT_Rank", "DT_Importance")]
          names(dt_imp) <- c("Variable", "Label", "Tree Rank", "Tree Importance")
        } else {
          dt_imp <- dt_imp[, c("Variable", "DT_Rank", "DT_Importance")]
          names(dt_imp) <- c("Variable", "Tree Rank", "Tree Importance")
        }

        dt_imp <- head(dt_imp, 15)
        rank_col <- if ("Label" %in% names(dt_imp)) "Tree Rank" else "Tree Rank"

        return(datatable(
          dt_imp,
          options = list(
            dom = 't',
            paging = FALSE,
            scrollX = TRUE,
            columnDefs = list(
              list(className = 'dt-center', targets = '_all'),
              # Hide importance column
              list(visible = FALSE, targets = if ("Label" %in% names(dt_imp)) 3 else 2)
            )
          ),
          class = 'compact hover',
          rownames = FALSE,
          caption = "Note: Random Forest comparison unavailable"
        ) %>%
          formatStyle(
            rank_col,
            backgroundColor = styleInterval(
              c(3, 6, 10),
              c('#d1fae5', '#fef3c7', '#fde68a', '#f1f0fb')
            ),
            fontWeight = styleInterval(c(3), c('bold', 'normal'))
          ) %>%
          formatStyle(
            'Variable',
            fontWeight = 'bold',
            fontSize = '0.9em'
          )
        )
      } else {
        return(NULL)
      }
    }

    # Decision Tree importance
    if (!is.null(rv$model$variable.importance)) {
      dt_imp <- data.frame(
        Variable = names(rv$model$variable.importance),
        DT_Importance = round(rv$model$variable.importance, 2),
        stringsAsFactors = FALSE
      )
      dt_imp$DT_Rank <- rank(-dt_imp$DT_Importance, ties.method = "min")
    } else {
      return(NULL)
    }

    # Random Forest importance
    if (rf$type == "classification") {
      rf_imp_raw <- importance(rf, type = 2)
    } else {
      rf_imp_raw <- importance(rf, type = 1)
    }
    rf_imp <- data.frame(
      Variable = rownames(rf_imp_raw),
      RF_Importance = round(rf_imp_raw[, 1], 2),
      stringsAsFactors = FALSE
    )
    rf_imp$RF_Rank <- rank(-rf_imp$RF_Importance, ties.method = "min")

    # Merge
    comparison <- merge(dt_imp, rf_imp, by = "Variable", all = TRUE)
    comparison[is.na(comparison)] <- 0

    # Add labels from dictionary
    if (!is.null(rv$data_dict)) {
      comparison$Label <- sapply(comparison$Variable, function(v) {
        match_idx <- match(v, rv$data_dict$variable)
        if (!is.na(match_idx)) rv$data_dict$label[match_idx] else ""
      })
      comparison <- comparison[, c("Variable", "Label", "DT_Rank", "RF_Rank", "DT_Importance", "RF_Importance")]
    } else {
      comparison <- comparison[, c("Variable", "DT_Rank", "RF_Rank", "DT_Importance", "RF_Importance")]
    }

    # Sort by average rank
    comparison$Avg_Rank <- (comparison$DT_Rank + comparison$RF_Rank) / 2
    comparison <- comparison[order(comparison$Avg_Rank), ]
    comparison$Avg_Rank <- NULL

    # Rename columns for display
    if ("Label" %in% names(comparison)) {
      names(comparison) <- c("Variable", "Label", "Tree Rank", "RF Rank", "Tree Imp", "RF Imp")
    } else {
      names(comparison) <- c("Variable", "Tree Rank", "RF Rank", "Tree Imp", "RF Imp")
    }

    # Limit to top 15 and prepare for display
    comparison <- head(comparison, 15)

    # Get rank columns for color coding
    tree_rank_col <- if ("Label" %in% names(comparison)) "Tree Rank" else "Tree Rank"
    rf_rank_col <- if ("Label" %in% names(comparison)) "RF Rank" else "RF Rank"

    datatable(
      comparison,
      options = list(
        dom = 't',
        paging = FALSE,
        scrollX = TRUE,
        columnDefs = list(
          list(className = 'dt-center', targets = '_all'),
          # Hide importance columns to keep it compact - ranks are more interpretable
          list(visible = FALSE, targets = if ("Label" %in% names(comparison)) c(4, 5) else c(3, 4))
        )
      ),
      class = 'compact hover',
      rownames = FALSE
    ) %>%
      formatStyle(
        tree_rank_col,
        backgroundColor = styleInterval(
          c(3, 6, 10),
          c('#d1fae5', '#fef3c7', '#fde68a', '#f1f0fb')  # green, yellow, light yellow, grey
        ),
        fontWeight = styleInterval(c(3), c('bold', 'normal'))
      ) %>%
      formatStyle(
        rf_rank_col,
        backgroundColor = styleInterval(
          c(3, 6, 10),
          c('#d1fae5', '#fef3c7', '#fde68a', '#f1f0fb')
        ),
        fontWeight = styleInterval(c(3), c('bold', 'normal'))
      ) %>%
      formatStyle(
        'Variable',
        fontWeight = 'bold',
        fontSize = '0.9em'
      )
  })

  # -------------------------------------------------------------------------
  # PowerPoint Export
  # -------------------------------------------------------------------------

  output$export_pptx <- downloadHandler(
    filename = function() {
      paste0("decision_tree_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".pptx")
    },
    content = function(file) {
      req(rv$model, rv$data, input$outcome_var)

      withProgress(message = "Generating PowerPoint...", value = 0, {

        # Create PowerPoint
        pptx <- read_pptx()

        # --- Slide 1: Title Slide ---
        incProgress(0.2, detail = "Creating title slide...")

        pptx <- add_slide(pptx, layout = "Title Slide", master = "Office Theme")
        pptx <- ph_with(pptx, value = "Decision Tree Analysis", location = ph_location_type(type = "ctrTitle"))
        pptx <- ph_with(pptx, value = paste0("Outcome: ", input$outcome_var, "\n", format(Sys.Date(), "%B %d, %Y")),
                        location = ph_location_type(type = "subTitle"))

        # --- Slide 2: Decision Tree Visualization ---
        incProgress(0.2, detail = "Adding tree visualization...")

        pptx <- add_slide(pptx, layout = "Title and Content", master = "Office Theme")
        pptx <- ph_with(pptx, value = "Decision Tree", location = ph_location_type(type = "title"))

        # Save tree plot to temp file
        tree_temp <- tempfile(fileext = ".png")
        png(tree_temp, width = 10, height = 7, units = "in", res = 150)
        # Use extra=104 for classification, extra=101 for regression/anova
        extra_val <- if (rv$model$method == "class") 104 else 101
        rpart.plot(
          rv$model,
          type = 4,
          extra = extra_val,
          under = TRUE,
          fallen.leaves = TRUE,
          roundint = FALSE,
          box.palette = "BuGn",
          shadow.col = "gray",
          main = ""
        )
        dev.off()

        pptx <- ph_with(pptx, value = external_img(tree_temp, width = 9, height = 6),
                        location = ph_location_type(type = "body"))

        # --- Slide 3: Variable Importance ---
        incProgress(0.2, detail = "Adding variable importance...")

        if (!is.null(rv$model$variable.importance)) {
          pptx <- add_slide(pptx, layout = "Title and Content", master = "Office Theme")
          pptx <- ph_with(pptx, value = "Variable Importance", location = ph_location_type(type = "title"))

          importance_df <- data.frame(
            Variable = names(rv$model$variable.importance),
            Importance = round(rv$model$variable.importance, 2)
          ) |>
            arrange(desc(Importance)) |>
            head(10)

          # Add labels from dictionary if available
          if (!is.null(rv$data_dict)) {
            importance_df$Label <- sapply(importance_df$Variable, function(v) {
              match_idx <- match(v, rv$data_dict$variable)
              if (!is.na(match_idx)) rv$data_dict$label[match_idx] else v
            })
            # Use labels for display
            importance_df$DisplayName <- importance_df$Label
          } else {
            importance_df$DisplayName <- importance_df$Variable
          }

          importance_df <- importance_df |>
            mutate(DisplayName = factor(DisplayName, levels = rev(DisplayName)))

          # Save importance plot to temp file
          imp_temp <- tempfile(fileext = ".png")
          png(imp_temp, width = 10, height = 6, units = "in", res = 150)
          p <- ggplot(importance_df, aes(x = Importance, y = DisplayName)) +
            geom_col(fill = "#6366f1") +
            theme_minimal(base_size = 14) +
            theme(
              axis.title.y = element_blank(),
              panel.grid.major.y = element_blank()
            ) +
            labs(x = "Importance Score")
          print(p)
          dev.off()

          pptx <- ph_with(pptx, value = external_img(imp_temp, width = 9, height = 5.5),
                          location = ph_location_type(type = "body"))
        }

        # --- Slide 4: Model Performance (for classification) ---
        incProgress(0.2, detail = "Adding model statistics...")

        if (rv$model$method == "class") {
          pptx <- add_slide(pptx, layout = "Title and Content", master = "Office Theme")
          pptx <- ph_with(pptx, value = "Model Performance", location = ph_location_type(type = "title"))

          pred <- predict(rv$model, type = "class")
          actual <- rv$data[[input$outcome_var]]
          cm <- table(Predicted = pred, Actual = actual)
          accuracy <- sum(diag(cm)) / sum(cm)

          # Create summary text
          n_nodes <- sum(rv$model$frame$var == "<leaf>")
          performance_text <- paste0(
            "Model Statistics\n\n",
            "• Accuracy: ", round(accuracy * 100, 1), "%\n",
            "• Terminal Nodes: ", n_nodes, "\n",
            "• Training Observations: ", nrow(rv$data), "\n",
            "• Complexity Parameter: ", input$cp, "\n",
            "• Minimum Bucket Size: ", input$minbucket, "\n",
            "• Maximum Depth: ", input$maxdepth
          )

          pptx <- ph_with(pptx, value = performance_text, location = ph_location_type(type = "body"))
        }

        # --- Slide 5: Key Decision Rules ---
        incProgress(0.1, detail = "Adding decision rules...")

        pptx <- add_slide(pptx, layout = "Title and Content", master = "Office Theme")
        pptx <- ph_with(pptx, value = "Decision Rules", location = ph_location_type(type = "title"))

        rules <- capture.output(rpart.rules(rv$model, style = "tall", cover = TRUE))
        rules_text <- paste(head(rules, 30), collapse = "\n")  # Limit to first 30 lines
        if (length(rules) > 30) {
          rules_text <- paste0(rules_text, "\n\n... (", length(rules) - 30, " more lines)")
        }

        pptx <- ph_with(pptx, value = rules_text, location = ph_location_type(type = "body"))

        # --- Slide 6: Disclaimer ---
        incProgress(0.1, detail = "Finalizing...")

        pptx <- add_slide(pptx, layout = "Title and Content", master = "Office Theme")
        pptx <- ph_with(pptx, value = "Notes & Disclaimer", location = ph_location_type(type = "title"))

        disclaimer_text <- paste0(
          "Analysis Details\n\n",
          "• Generated: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n",
          "• Tool: Decision Tree Analysis\n\n",
          "Disclaimer\n\n",
          "This analysis is provided for informational purposes. ",
          "Results should be validated by qualified analysts and interpreted ",
          "in the context of the specific research objectives and data limitations. ",
          "Decision trees are sensitive to the data used and parameter settings."
        )

        pptx <- ph_with(pptx, value = disclaimer_text, location = ph_location_type(type = "body"))

        # Save the file
        print(pptx, target = file)

        # Clean up temp files
        unlink(tree_temp)
        if (exists("imp_temp")) unlink(imp_temp)
      })
    }
  )

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

      # Define persona-specific instructions
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
                                     # Default fallback
                                     paste0(
                                       "\n\nRESPONSE STYLE:\n",
                                       "- Be clear and concise\n",
                                       "- Use plain language\n",
                                       "- Focus on actionable insights\n",
                                       "- Do not offer to run additional analyses"
                                     )
      )

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
        "explain what additional analysis might be needed.",
        persona_instructions
      )

      # Create chat based on provider with validation
      if (input$ai_provider == "azure") {
        # Azure OpenAI (secure company endpoint)
        model <- input$ai_model
        if (model == "" || is.null(model)) {
          model <- Sys.getenv("AZURE_OPENAI_DEPLOYMENT")
        }
        api_key <- Sys.getenv("AZURE_OPENAI_API_KEY")
        endpoint <- Sys.getenv("AZURE_OPENAI_ENDPOINT")

        # Check for missing credentials
        missing_vars <- c()
        if (api_key == "") missing_vars <- c(missing_vars, "AZURE_OPENAI_API_KEY")
        if (endpoint == "") missing_vars <- c(missing_vars, "AZURE_OPENAI_ENDPOINT")
        if (model == "") missing_vars <- c(missing_vars, "AZURE_OPENAI_DEPLOYMENT")

        if (length(missing_vars) > 0) {
          stop(paste0(
            "Azure OpenAI is not configured. Missing environment variables:\n\n",
            paste("•", missing_vars, collapse = "\n"),
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
        # Check if Ollama is likely running
        rv$chat <- tryCatch({
          chat_ollama(
            model = input$ai_model,
            system_prompt = system_prompt
          )
        }, error = function(e) {
          stop(paste0(
            "Could not connect to Ollama. Please ensure:\n\n",
            "• Ollama is installed and running locally\n",
            "• The model '", input$ai_model, "' is downloaded\n",
            "• Ollama is accessible at http://localhost:11434\n\n",
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
        rv$chat <- chat_anthropic(
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
        rv$chat <- chat_google_gemini(
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

    # Try to get or create chat - handle configuration errors
    chat <- tryCatch({
      get_chat()
    }, error = function(e) {
      # Configuration error (missing API key, etc.)
      rv$chat_history <- c(rv$chat_history, list(
        list(role = "assistant", content = paste0(
          "⚠️ **AI Configuration Error**\n\n",
          e$message,
          "\n\nPlease check your settings and try again."
        ))
      ))
      return(NULL)
    })

    if (is.null(chat)) return(invisible(NULL))

    tryCatch({
      # Get AI response
      response <- chat$chat(message)

      # Add assistant response to history
      rv$chat_history <- c(rv$chat_history, list(
        list(role = "assistant", content = response)
      ))

    }, error = function(e) {
      # Parse common error types for user-friendly messages
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
        list(role = "assistant", content = paste0("⚠️ ", user_message))
      ))

      showNotification(
        "AI request failed. See chat for details.",
        type = "error",
        duration = 5
      )
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

}

# ============================================================================
# Run Application
# ============================================================================

shinyApp(ui = ui, server = server)
