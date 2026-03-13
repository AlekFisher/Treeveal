# mod_methodology.R
# Methodology Audit tab: Exposes the deterministic mathematical engine

library(shiny)
library(bslib)
library(utils)

methodology_ui <- function(id) {
  ns <- NS(id)
  
  layout_columns(
    col_widths = c(12),
    card(
      card_header(
        class = "d-flex align-items-center bg-light text-dark",
        bsicons::bs_icon("diagram-3-fill", class = "me-2"),
        "Methodology Audit & Deterministic Guarantees"
      ),
      card_body(
        div(
          class = "alert alert-success mb-4",
          h5(bsicons::bs_icon("shield-lock"), " AI Hallucination Guardrail Active"),
          p("All mathematical and statistical calculations in this application are performed deterministically using established R packages. The Artificial Intelligence algorithms deployed here do ", tags$strong("not"), " compute math, select variables, or build models. The AI is restricted strictly to generating natural language interpretations of the verified statistical output.")
        ),
        
        layout_columns(
          col_widths = c(6, 6),
          card(
            card_header("Statistical Engine"),
            card_body(
              tags$ul(
                class = "list-unstyled",
                tags$li(tags$strong("Environment: "), R.version.string),
                tags$li(tags$strong("Decision Tree Algorithm: "), "Recursive Partitioning and Regression Trees (", code("rpart"), " v", as.character(packageVersion("rpart")), ")"),
                tags$li(tags$strong("Validation Ensemble: "), "Breiman and Cutler's Random Forests (", code("randomForest"), " v", as.character(packageVersion("randomForest")), ")"),
                tags$li(tags$strong("Evaluation Metric: "), "Gini impurity (classification) or ANOVA/MSE (regression) explicitly calculated to determine optimal node splits.")
              )
            )
          ),
          card(
            card_header("Model Hyperparameters Applied"),
            card_body(
              conditionalPanel(
                condition = "output.model_built",
                uiOutput(ns("hyperparameters"))
              ),
              conditionalPanel(
                condition = "!output.model_built",
                p(class = "text-muted", "Build a decision tree to audit applied parameters.")
              )
            )
          )
        )
      )
    )
  )
}

methodology_server <- function(id, rv) {
  moduleServer(id, function(input, output, session) {
    output$hyperparameters <- renderUI({
      req(rv$model)
      
      tags$ul(
        tags$li(tags$strong("Complexity Parameter (cp): "), rv$cp, " - minimum improvement in fit needed to attempt a split."),
        tags$li(tags$strong("Minimum Bucket Size: "), rv$minbucket, " - smallest allowable number of observations in any terminal node."),
        tags$li(tags$strong("Maximum Depth: "), rv$maxdepth, " - maximum levels of separation from the root node."),
        tags$li(tags$strong("Method Algorithm: "), rv$model$method)
      )
    })
  })
}
