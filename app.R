# Dendro: AI-Powered Decision Tree Analysis
# An interactive Shiny application for building, visualizing, and interpreting decision trees
# with AI assistance via ellmer
#
# Modules are auto-loaded from R/ directory:
#   utils_data.R, utils_model.R, utils_demo_data.R
#   mod_data_import.R, mod_factor_editor.R, mod_tree_config.R
#   mod_data_quality.R, mod_tree_viz.R, mod_model_details.R
#   mod_ai_chat.R, mod_export.R, mod_guide.R

library(shiny)
library(bslib)
library(rpart)
library(rpart.plot)
library(ellmer)
library(DT)
library(gt)
library(dplyr)
library(tidyr)
library(ggplot2)
library(readxl)
library(haven)
library(officer)
library(randomForest)
library(rvg)
library(visNetwork)

# ============================================================================
# PRODUCTION MODE TOGGLE
# ============================================================================
PRODUCTION_MODE <- as.logical(Sys.getenv("PRODUCTION_MODE", "TRUE"))

# ============================================================================
# UI
# ============================================================================

ui <- page_sidebar(
  title = tags$span(
    "Dendro",
    if (PRODUCTION_MODE) {
      tagList(
        tags$span(
          class = "badge bg-success ms-2",
          style = "font-size: 0.6em; vertical-align: middle;",
          "PRODUCTION"
        ),
        actionLink(
          "show_compliance",
          tags$span(
            class = "badge bg-info ms-2",
            style = "font-size: 0.6em; vertical-align: middle;",
            bsicons::bs_icon("shield-lock-fill"), " ZERO DATA RETENTION"
          ),
          style = "text-decoration: none;"
        )
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
    primary = "#2563eb",
    secondary = "#737373",
    success = "#059669",
    info = "#0284c7",
    warning = "#d97706",
    danger = "#dc2626",
    base_font = font_google("Inter"),
    heading_font = font_google("Inter"),
    code_font = font_google("JetBrains Mono"),
    "body-bg" = "#ffffff",
    "card-bg" = "#ffffff",
    "border-radius" = "8px",
    "border-color" = "#e5e5e5",
    "input-border-color" = "#e5e5e5",
    "card-border-color" = "#f0f0f0"
  ),

  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "theme.css"),
    tags$style(HTML("
      #shiny-disconnected-overlay, #ss-connect-dialog { display: none !important; }
    "))
  ),

  # Sidebar
  sidebar = sidebar(
    width = 350,
    title = "Configuration",

    accordion(
      id = "sidebar_accordion",
      open = c("data_panel", "model_panel"),

      accordion_panel(
        title = "Data Upload",
        value = "data_panel",
        icon = bsicons::bs_icon("cloud-upload"),
        data_import_ui("data")
      ),

      accordion_panel(
        title = "Factor Levels",
        value = "factor_panel",
        icon = bsicons::bs_icon("list-ol"),
        factor_editor_ui("factors")
      ),

      accordion_panel(
        title = "Model Parameters",
        value = "model_panel",
        icon = bsicons::bs_icon("sliders"),
        tree_config_ui("config")
      ),

      accordion_panel(
        title = "AI Configuration",
        value = "ai_panel",
        icon = bsicons::bs_icon("robot"),
        ai_chat_sidebar_ui("ai", PRODUCTION_MODE)
      )
    )
  ),

  # Main content
  navset_card_tab(
    id = "main_tabs",

    nav_panel(
      title = "Data Preview",
      icon = bsicons::bs_icon("table"),
      data_import_tab_ui("data")
    ),

    nav_panel(
      title = "Data Quality",
      data_quality_ui("quality")
    ),

    nav_panel(
      title = "Decision Tree",
      icon = bsicons::bs_icon("diagram-3"),
      tree_viz_ui("tree")
    ),

    nav_panel(
      title = "Model Details",
      icon = bsicons::bs_icon("file-earmark-code"),
      model_details_ui("details")
    ),

    nav_panel(
      title = "Bivariate Analysis",
      icon = bsicons::bs_icon("bar-chart-steps"),
      bivariate_ui("bivariate")
    ),

    nav_panel(
      title = "Methodology Audit",
      icon = bsicons::bs_icon("clipboard-data"),
      methodology_ui("methodology")
    ),

    nav_panel(
      title = "Guide",
      icon = bsicons::bs_icon("book"),
      guide_ui("guide")
    )
  )
)

# ============================================================================
# Server
# ============================================================================

server <- function(input, output, session) {

  # Shared reactive state
  rv <- reactiveValues(
    data = NULL,
    data_dict = NULL,
    model = NULL,
    chat_history = list(),
    chat = NULL,
    show_ai_panel = FALSE,
    factor_levels_pending = list(),
    is_demo_data = FALSE,
    # Cross-module state (set by modules, read by others)
    outcome_var = NULL,
    predictor_vars = NULL,
    cp = 0.01,
    minbucket = 30,
    maxdepth = 4
  )

  # Global conditional panel outputs (used by conditionalPanel across all modules)
  output$data_loaded <- reactive({ !is.null(rv$data) })
  outputOptions(output, "data_loaded", suspendWhenHidden = FALSE)

  output$model_built <- reactive({ !is.null(rv$model) })
  outputOptions(output, "model_built", suspendWhenHidden = FALSE)

  # Compliance popup
  observeEvent(input$show_compliance, {
    showModal(modalDialog(
      title = tagList(bsicons::bs_icon("shield-check", class = "text-success me-2"), "Data Governance & Security"),
      p(class = "lead", "Your data remains strictly secure and private."),
      tags$ul(
        tags$li(tags$strong("Local Processing: "), "All raw, row-level data is processed entirely within this application's secure statistical engine."),
        tags$li(tags$strong("Zero Data Retention: "), "When using AI interpretation features, only anonymized model metadata (decision rules, variable names, and statistical metrics) is sent to secure enterprise endpoints."),
        tags$li(tags$strong("No Public Model Training: "), "Your data and tree parameters are never used to train public foundation models.")
      ),
      p(class = "text-muted small mt-3", "Enterprise Compliance Mode Active: Azure OpenAI / Local deployment enforced."),
      easyClose = TRUE,
      footer = modalButton("Close"),
      size = "l"
    ))
  })

  # Module servers
  data_import_server("data", rv)
  factor_editor_server("factors", rv)
  tree_config_server("config", rv, parent_session = session)
  data_quality_server("quality", rv)
  tree_viz_server("tree", rv, ai_card_fn = function() ai_chat_card_ui("ai"))
  model_details_server("details", rv)
  bivariate_server("bivariate", rv, active_tab = reactive(input$main_tabs))
  methodology_server("methodology", rv)
  ai_chat_server("ai", rv, PRODUCTION_MODE)
  # guide_ui is pure UI — no server needed
}

# ============================================================================
# Run Application
# ============================================================================

shinyApp(ui = ui, server = server)
