# Dendro: AI-Powered Decision Tree Analysis
# An interactive Shiny application for building, visualizing, and interpreting decision trees
# with AI assistance via ellmer
#
# Modules are auto-loaded from R/ directory:
  #   utils_data.R, utils_data_filters.R, utils_model.R, utils_demo_data.R
  #   mod_data_import.R, mod_data_filter.R, mod_factor_editor.R, mod_tree_config.R
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
library(cicerone)

# ============================================================================
# PRODUCTION MODE TOGGLE
# ============================================================================
PRODUCTION_MODE <- as.logical(Sys.getenv("PRODUCTION_MODE", "TRUE"))

# ============================================================================
# UI
# ============================================================================

ui <- page_sidebar(
  title = tags$div(
    id = "app_title_area",
    style = "display: flex; align-items: center; justify-content: space-between; width: 100%;",
    tags$div(
      style = "display: flex; align-items: center; gap: 1rem;",
      tags$span(
        style = "font-family: Georgia, 'Times New Roman', serif; font-size: 1.5rem; font-weight: 500;",
        "Dendro"
      ),
      tags$span(
        style = "color: rgba(255,255,255,0.7); font-size: 0.875rem; font-weight: 400; border-left: 1px solid rgba(255,255,255,0.3); padding-left: 1rem;",
        "Decision Tree Analysis"
      ),
      if (PRODUCTION_MODE) {
        tagList(
          tags$span(
            class = "badge bg-success",
            "PRODUCTION"
          ),
          actionLink(
            "show_compliance",
            tags$span(
              class = "badge bg-info",
              bsicons::bs_icon("shield-lock-fill"), " ZERO DATA RETENTION"
            ),
            style = "text-decoration: none;"
          )
        )
      } else {
        tags$span(
          class = "badge bg-warning",
          "DEV"
        )
      }
    ),
    tags$div(
      style = "display: flex; align-items: center; gap: 0.75rem;",
      actionButton("start_tour", "Guided Tour", class = "btn-outline-light btn-sm", icon = icon("route")),
      tags$span(
        style = "color: rgba(255,255,255,0.9); font-size: 0.75rem; font-weight: 600; letter-spacing: 0.05em;",
        "ADELPHI RESEARCH"
      )
    )
  ),
  theme = bs_theme(
    version = 5,
    bootswatch = "default",
    primary = "#00A6AD",  # Adelphi teal
    secondary = "#51626F", # Adelphi secondary text
    success = "#059669",
    info = "#0284c7",
    warning = "#F2B82D",  # Adelphi yellow
    danger = "#dc2626",
    base_font = font_collection(font_google("Open Sans"), "Segoe UI", "sans-serif"),  # Aptos-like fallback
    heading_font = font_collection("Georgia", "Times New Roman", "serif"),
    code_font = font_google("JetBrains Mono"),
    "body-bg" = "#F3FBFA",  # Adelphi soft background
    "card-bg" = "#FFFFFF",
    "border-radius" = "18px",
    "border-color" = "rgba(0, 89, 110, 0.10)",
    "input-border-color" = "rgba(0, 89, 110, 0.10)",
    "card-border-color" = "rgba(0, 89, 110, 0.10)"
  ),
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "theme.css"),
    tags$style(HTML("
      #shiny-disconnected-overlay, #ss-connect-dialog { display: none !important; }
    "))
  ),
  use_cicerone(),

  # Sidebar
  sidebar = sidebar(
    width = 360,
    tags$div(
      style = "padding: 0 0.5rem 1rem 0.5rem;",
      tags$div(
        style = "font-family: Georgia, 'Times New Roman', serif; font-size: 1.25rem; font-weight: 500; color: #24313B; margin-bottom: 0.5rem;",
        "Analysis Setup"
      ),
      tags$p(
        style = "font-size: 0.8125rem; color: #51626F; margin: 0; line-height: 1.5;",
        "Configure your decision tree analysis step-by-step"
      )
    ),
    accordion(
      id = "sidebar_accordion",
      open = c("data_panel"),
      accordion_panel(
        title = "1. Data",
        value = "data_panel",
        icon = bsicons::bs_icon("cloud-upload"),
        data_import_ui("data")
      ),
      accordion_panel(
        title = "2. Filter (Optional)",
        value = "filter_panel",
        icon = bsicons::bs_icon("funnel"),
        data_filter_ui("filter")
      ),
      accordion_panel(
        title = "3. Model Settings",
        value = "model_panel",
        icon = bsicons::bs_icon("sliders"),
        tree_config_ui("config")
      ),
      accordion_panel(
        title = "Advanced: Factor Levels",
        value = "factor_panel",
        icon = bsicons::bs_icon("list-ol"),
        factor_editor_ui("factors")
      ),
      accordion_panel(
        title = "AI Assistant",
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
    data_source = NULL,
    data_raw = NULL,
    data = NULL,
    data_dict = NULL,
    model = NULL,
    chat_history = list(),
    chat = NULL,
    show_ai_panel = FALSE,
    active_filter = NULL,
    applied_factor_levels = list(),
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
  output$data_loaded <- reactive({
    !is.null(rv$data)
  })
  outputOptions(output, "data_loaded", suspendWhenHidden = FALSE)

  output$model_built <- reactive({
    !is.null(rv$model)
  })
  outputOptions(output, "model_built", suspendWhenHidden = FALSE)

  # Cicerone guided tour
  guide <- Cicerone$
    new()$
    step(
    el = "app_title_area",
    title = "Welcome to Dendro",
    description = "Dendro makes decision tree analysis accessible and easy to interpret."
  )$
    step(
    el = "[data-value='data_panel']",
    is_id = FALSE,
    title = "Data Upload & Quality Gatekeeper",
    description = "We ensure your data is robust before modeling. Upload your data here to get started."
  )$
    step(
    el = "[data-value='model_panel']",
    is_id = FALSE,
    title = "Model Parameters",
    description = "Take full control over the decision tree algorithm with transparent settings."
  )$
    step(
    el = "a[data-value='Decision Tree']",
    is_id = FALSE,
    title = "Decision Tree Visualization",
    description = "Explore the deterministic outcomes of your model visually."
  )$
    step(
    el = "[data-value='ai_panel']",
    is_id = FALSE,
    title = "AI Configuration (Zero Retention)",
    description = "Get secure, context-aware AI interpretations of your model here. Your row-level data is never sent to the AI."
  )

  guide$init()

  observeEvent(input$start_tour, {
    guide$start()
  })

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
      p(class = "text-muted small mt-3", "Enterprise Compliance Mode Active: Azure-backed deployments enforced."),
      easyClose = TRUE,
      footer = modalButton("Close"),
      size = "l"
    ))
  })

  # Module servers
  data_import_server("data", rv)
  data_filter_server("filter", rv)
  factor_editor_server("factors", rv)
  tree_config_server("config", rv, parent_session = session)
  data_quality_server("quality", rv)
  tree_viz_server("tree", rv, ai_card_fn = function() ai_chat_card_ui("ai"))
  model_details_server("details", rv)
  bivariate_server("bivariate", rv, active_tab = reactive(input$main_tabs))
  methodology_server("methodology", rv)
  ai_chat_server("ai", rv, PRODUCTION_MODE)
  # guide_ui is pure UI — no server needed

  observe({
    raw_data <- rv$data_raw

    if (is.null(raw_data)) {
      rv$data <- NULL
    } else {
      rv$data <- apply_single_filter(raw_data, rv$active_filter)
    }
  })
}

# ============================================================================
# Run Application
# ============================================================================

shinyApp(ui = ui, server = server)
