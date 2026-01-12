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

# ============================================================================
# UI
# ============================================================================

ui <- page_sidebar(
  title = tags$span(
    tags$span("🌳", style = "margin-right: 8px;"),
    "Treeveal"
  ),
  theme = bs_theme(
    version = 5,
    bootswatch = "flatly",
    primary = "#2c3e50",
    secondary = "#18bc9c",
    success = "#18bc9c",
    info = "#3498db",
    warning = "#f39c12",
    danger = "#e74c3c",
    base_font = font_google("Source Sans Pro"),
    heading_font = font_google("Poppins"),
    code_font = font_google("Fira Code"),
    "navbar-bg" = "#2c3e50"
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
          "Upload Dataset (CSV)",
          accept = c(".csv", ".CSV"),
          placeholder = "Choose a CSV file..."
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
          value = 5,
          step = 1
        ),
        helpText("Minimum observations in terminal nodes"),

        sliderInput(
          "maxdepth",
          "Maximum Depth",
          min = 1,
          max = 30,
          value = 10,
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

        selectInput(
          "ai_provider",
          "AI Provider",
          choices = c(
            "Anthropic (Claude)" = "anthropic",
            "Google (Gemini)" = "gemini",
            "OpenAI (GPT)" = "openai"
          ),
          selected = "anthropic"
        ),

        selectInput(
          "ai_model",
          "Model",
          choices = c(
            "claude-haiku-4-5-20251001" = "claude-haiku-4-5-20251001"
          ),
          selected = "claude-haiku-4-5-20251001"
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
            class = "bg-primary text-white",
            "Dataset Overview"
          ),
          card_body(
            conditionalPanel(
              condition = "!output.data_loaded",
              div(
                class = "text-center py-5",
                bsicons::bs_icon("cloud-upload", size = "4rem", class = "text-muted"),
                h4(class = "text-muted mt-3", "Upload a dataset to begin"),
                p(class = "text-muted", "CSV files with headers are supported")
              )
            ),
            conditionalPanel(
              condition = "output.data_loaded",
              layout_columns(
                col_widths = c(4, 4, 4),
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
                )
              ),
              hr(),
              DTOutput("data_preview")
            )
          )
        )
      )
    ),

    # Decision Tree Tab
    nav_panel(
      title = "Decision Tree",
      icon = bsicons::bs_icon("diagram-3"),

      layout_columns(
        col_widths = c(8, 4),

        card(
          card_header(
            class = "bg-primary text-white",
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

        card(
          card_header(
            class = "bg-secondary text-white",
            "Model Summary"
          ),
          card_body(
            conditionalPanel(
              condition = "output.model_built",

              h5("Variable Importance"),
              plotOutput("var_importance", height = "200px"),

              hr(),

              h5("Model Statistics"),
              verbatimTextOutput("model_stats"),

              hr(),

              h5("Confusion Matrix"),
              DTOutput("confusion_matrix")
            ),
            conditionalPanel(
              condition = "!output.model_built",
              div(
                class = "text-center py-3 text-muted",
                "Build a model to see summary statistics"
              )
            )
          )
        )
      )
    ),

    # AI Interpretation Tab
    nav_panel(
      title = "AI Interpretation",
      icon = bsicons::bs_icon("chat-dots"),

      layout_columns(
        col_widths = c(12),

        card(
          card_header(
            class = "bg-primary text-white d-flex justify-content-between align-items-center",
            span("AI-Powered Analysis"),
            actionButton(
              "clear_chat",
              "Clear Chat",
              class = "btn-sm btn-outline-light",
              icon = icon("trash")
            )
          ),
          card_body(
            min_height = "500px",

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

              # Chat history display
              div(
                id = "chat_container",
                style = "height: 400px; overflow-y: auto; border: 1px solid #dee2e6; border-radius: 8px; padding: 15px; margin-bottom: 15px; background-color: #f8f9fa;",
                uiOutput("chat_history")
              ),

              # Quick action buttons
              div(
                class = "mb-3",
                actionButton("ask_interpret", "Interpret Tree", class = "btn-outline-primary btn-sm me-2", icon = icon("lightbulb")),
                actionButton("ask_insights", "Key Insights", class = "btn-outline-primary btn-sm me-2", icon = icon("chart-line")),
                actionButton("ask_recommendations", "Recommendations", class = "btn-outline-primary btn-sm me-2", icon = icon("list-check")),
                actionButton("ask_limitations", "Limitations", class = "btn-outline-primary btn-sm", icon = icon("exclamation-triangle"))
              ),

              # User input
              layout_columns(
                col_widths = c(10, 2),
                textAreaInput(
                  "user_message",
                  NULL,
                  placeholder = "Ask questions about your decision tree...",
                  rows = 2
                ),
                actionButton(
                  "send_message",
                  "Send",
                  class = "btn-primary btn-lg w-100 h-100",
                  icon = icon("paper-plane")
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

        card(
          card_header(class = "bg-primary text-white", "Tree Rules"),
          card_body(
            verbatimTextOutput("tree_rules")
          )
        ),

        card(
          card_header(class = "bg-primary text-white", "CP Table"),
          card_body(
            DTOutput("cp_table")
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
      rv$data <- read.csv(input$data_file$datapath, stringsAsFactors = TRUE)
      rv$model <- NULL
      rv$chat_history <- list()
      rv$chat <- NULL

      showNotification("Data loaded successfully!", type = "message")
    }, error = function(e) {
      showNotification(paste("Error loading data:", e$message), type = "error")
    })
  })

  # Load demo data
  observeEvent(input$use_demo, {
    if (input$use_demo) {
      # Create HCP GLP-1 Prescribing Survey dataset
      # Attitudes and beliefs predicting prescribing behavior
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

      # === Attitudinal Statements (5-point Likert: 1=Strongly Disagree to 5=Strongly Agree) ===
      # Create correlated attitudes that form natural segments

      # Underlying latent "enthusiast" trait
      enthusiast_trait <- rnorm(n, 0, 1)

      # Underlying latent "barrier sensitivity" trait
      barrier_trait <- rnorm(n, 0, 1)

      # Efficacy beliefs - correlated with enthusiast trait
      att_glp1_effective_a1c <- pmin(5, pmax(1, round(3.5 + 0.7 * enthusiast_trait + rnorm(n, 0, 0.8))))
      att_glp1_effective_weight <- pmin(5, pmax(1, round(3.4 + 0.8 * enthusiast_trait + rnorm(n, 0, 0.8))))
      att_glp1_cv_benefit <- pmin(5, pmax(1, round(3.0 + 0.5 * enthusiast_trait + rnorm(n, 0, 1))))

      # Safety/tolerability - mixed relationships
      att_gi_side_effects_concern <- pmin(5, pmax(1, round(3.0 - 0.3 * enthusiast_trait + 0.4 * barrier_trait + rnorm(n, 0, 0.9))))
      att_pancreatitis_concern <- pmin(5, pmax(1, round(2.5 + 0.5 * barrier_trait + rnorm(n, 0, 1))))
      att_patient_tolerability <- pmin(5, pmax(1, round(3.2 + 0.6 * enthusiast_trait - 0.3 * barrier_trait + rnorm(n, 0, 0.8))))

      # Access/cost barriers - correlated with barrier trait
      att_cost_barrier <- pmin(5, pmax(1, round(3.3 + 0.7 * barrier_trait + rnorm(n, 0, 0.8))))
      att_pa_burden <- pmin(5, pmax(1, round(3.5 + 0.6 * barrier_trait + rnorm(n, 0, 0.9))))
      att_patient_afford <- pmin(5, pmax(1, round(2.8 - 0.6 * barrier_trait + rnorm(n, 0, 0.9))))

      # Treatment philosophy
      att_early_intervention <- pmin(5, pmax(1, round(3.2 + 0.6 * enthusiast_trait + rnorm(n, 0, 0.9))))
      att_weight_tx_priority <- pmin(5, pmax(1, round(3.3 + 0.5 * enthusiast_trait + rnorm(n, 0, 0.9))))
      att_prefer_oral_first <- pmin(5, pmax(1, round(3.0 - 0.5 * enthusiast_trait + 0.3 * barrier_trait + rnorm(n, 0, 1))))

      # Experience and confidence - key differentiators
      att_comfortable_initiating <- pmin(5, pmax(1, round(3.0 + 0.8 * enthusiast_trait - 0.2 * barrier_trait + rnorm(n, 0, 0.8))))
      att_adequate_training <- pmin(5, pmax(1, round(3.0 + 0.5 * enthusiast_trait + rnorm(n, 0, 1))))

      # Influence and information
      att_trust_clinical_data <- pmin(5, pmax(1, round(3.5 + 0.4 * enthusiast_trait + rnorm(n, 0, 0.8))))
      att_peer_influence <- pmin(5, pmax(1, round(3.0 + rnorm(n, 0, 1))))
      att_guideline_adherence <- pmin(5, pmax(1, round(3.5 + 0.3 * enthusiast_trait + rnorm(n, 0, 0.9))))

      # Patient factors
      att_patients_interested <- pmin(5, pmax(1, round(3.2 + 0.4 * enthusiast_trait + rnorm(n, 0, 1))))
      att_patients_compliant <- pmin(5, pmax(1, round(2.9 + 0.3 * enthusiast_trait - 0.2 * barrier_trait + rnorm(n, 0, 1))))

      # === Generate Prescribing Outcome ===
      # Create clear decision boundaries for interpretable tree

      prescribe_score <-
        # Comfort is the primary driver
        0.35 * (att_comfortable_initiating - 3) +
        # Efficacy beliefs matter
        0.20 * (att_glp1_effective_weight - 3) +
        0.15 * (att_glp1_effective_a1c - 3) +
        # Barriers reduce prescribing
        -0.25 * (att_cost_barrier - 3) +
        -0.15 * (att_prefer_oral_first - 3) +
        # Philosophy matters
        0.20 * (att_early_intervention - 3) +
        0.15 * (att_weight_tx_priority - 3) +
        # Safety concerns reduce
        -0.10 * (att_gi_side_effects_concern - 3) +
        # Specialty effect
        0.5 * (specialty == "Endocrinology") +
        0.2 * (specialty == "Internal Medicine") +
        # Add noise
        rnorm(n, 0, 0.5)

      # Create ~45% high prescribers for balanced classes
      threshold <- quantile(prescribe_score, 0.55)

      high_prescriber <- factor(
        ifelse(prescribe_score > threshold, "High", "Low"),
        levels = c("Low", "High")
      )

      hcp_data <- data.frame(
        # Outcome
        High_Prescriber = high_prescriber,

        # Demographics
        Specialty = factor(specialty),
        Years_in_Practice = years_practice,
        Practice_Setting = factor(practice_setting),
        Region = factor(region),

        # Efficacy beliefs
        Effective_for_A1C = att_glp1_effective_a1c,
        Effective_for_Weight = att_glp1_effective_weight,
        CV_Benefit_Belief = att_glp1_cv_benefit,

        # Safety concerns
        GI_Side_Effect_Concern = att_gi_side_effects_concern,
        Pancreatitis_Concern = att_pancreatitis_concern,
        Patient_Tolerability = att_patient_tolerability,

        # Access barriers
        Cost_is_Barrier = att_cost_barrier,
        Prior_Auth_Burden = att_pa_burden,
        Patients_Can_Afford = att_patient_afford,

        # Treatment philosophy
        Prefer_Early_Intervention = att_early_intervention,
        Weight_Tx_Priority = att_weight_tx_priority,
        Prefer_Oral_First = att_prefer_oral_first,

        # Confidence
        Comfortable_Initiating = att_comfortable_initiating,
        Adequate_Training = att_adequate_training,

        # Influence
        Trust_Clinical_Data = att_trust_clinical_data,
        Peer_Influence = att_peer_influence,
        Follow_Guidelines = att_guideline_adherence,

        # Patient factors
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
    if (input$ai_provider == "anthropic") {
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

  # Helper function to get model description for AI
  get_model_context <- reactive({
    req(rv$model, rv$data, input$outcome_var)

    # Capture tree rules
    rules <- capture.output(rpart.rules(rv$model, style = "tall", cover = TRUE))

    # Variable importance
    var_imp <- if (!is.null(rv$model$variable.importance)) {
      paste(
        names(rv$model$variable.importance),
        ":",
        round(rv$model$variable.importance, 2),
        collapse = "\n"
      )
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
      "- Maximum Depth: ", input$maxdepth
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
        "\n\nProvide clear, actionable insights. Use specific numbers from the model when relevant. ",
        "Be conversational but precise. If the user asks about something not shown in the model, ",
        "explain what additional analysis might be needed."
      )

      # Create chat based on provider
      if (input$ai_provider == "anthropic") {
        rv$chat <- chat_anthropic(
          model = input$ai_model,
          system_prompt = system_prompt
        )
      } else if (input$ai_provider == "gemini") {
        rv$chat <- chat_gemini(
          model = input$ai_model,
          system_prompt = system_prompt
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
