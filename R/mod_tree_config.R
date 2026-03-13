# mod_tree_config.R
# Sidebar panel for model parameters + Build button

tree_config_ui <- function(id) {
  ns <- NS(id)

  tagList(
    sliderInput(
      ns("cp"),
      "Complexity Parameter (cp)",
      min = 0.001,
      max = 0.1,
      value = 0.01,
      step = 0.001
    ),
    helpText("Lower values = more complex trees"),

    sliderInput(
      ns("minbucket"),
      "Minimum Bucket Size",
      min = 1,
      max = 50,
      value = 30,
      step = 1
    ),
    helpText("Minimum observations in terminal nodes"),

    sliderInput(
      ns("maxdepth"),
      "Maximum Depth",
      min = 1,
      max = 30,
      value = 4,
      step = 1
    ),

    hr(),

    uiOutput(ns("build_btn_container"))
  )
}

tree_config_server <- function(id, rv, parent_session) {
  moduleServer(id, function(input, output, session) {

    ns <- session$ns

    # Sync parameters to rv for cross-module access
    observe({ rv$cp <- input$cp })
    observe({ rv$minbucket <- input$minbucket })
    observe({ rv$maxdepth <- input$maxdepth })

    output$build_btn_container <- renderUI({
      is_critical <- isTRUE(rv$data_health_critical)
      
      if (is_critical) {
        div(
          actionButton(
            ns("run_model_disabled"),
            "Build Decision Tree",
            class = "btn-secondary btn-lg w-100 disabled",
            icon = icon("ban")
          ),
          p(class = "text-danger small mt-2 text-center", bsicons::bs_icon("exclamation-triangle"), " Address critical Data Quality issues first")
        )
      } else {
        actionButton(
          ns("run_model"),
          "Build Decision Tree",
          class = "btn-primary btn-lg w-100",
          icon = icon("play")
        )
      }
    })

    # Model building
    observeEvent(input$run_model, {
      req(rv$data, rv$outcome_var, rv$predictor_vars)

      if (nrow(rv$data) == 0) {
        showNotification("The active filter removed all rows. Clear or widen the filter and try again.", type = "error")
        return()
      }

      # Validate predictor variables don't include outcome
      predictors <- setdiff(rv$predictor_vars, rv$outcome_var)

      if (length(predictors) == 0) {
        showNotification("Please select at least one predictor variable", type = "error")
        return()
      }

      outcome_values <- unique(stats::na.omit(rv$data[[rv$outcome_var]]))
      if (length(outcome_values) < 2) {
        showNotification(
          "The filtered dataset has fewer than two outcome values. Clear or widen the filter before building the tree.",
          type = "error"
        )
        return()
      }

      withProgress(message = "Building decision tree...", {

        tryCatch({
          rv$model <- build_decision_tree(
            data = rv$data,
            outcome_var = rv$outcome_var,
            predictors = predictors,
            cp = input$cp,
            minbucket = input$minbucket,
            maxdepth = input$maxdepth
          )

          # Reset chat when model changes
          rv$chat_history <- list()
          rv$chat <- NULL

          showNotification("Decision tree built successfully!", type = "message")

          # Switch to tree tab (uses parent session since main_tabs is not namespaced)
          updateNavlistPanel(parent_session, "main_tabs", selected = "Decision Tree")

        }, error = function(e) {
          showNotification(paste("Error building model:", e$message), type = "error")
        })
      })
    })
  })
}
