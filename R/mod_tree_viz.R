# mod_tree_viz.R
# Decision Tree tab: toggle button, dynamic tree+AI layout, tree plot

tree_viz_ui <- function(id) {
  ns <- NS(id)

  tagList(
    # Toggle button for AI panel
    div(
      class = "mb-2 d-flex justify-content-end",
      actionButton(
        ns("toggle_ai_panel"),
        "Show AI Chat",
        class = "btn-sm btn-outline-secondary",
        icon = icon("robot")
      )
    ),

    uiOutput(ns("tree_layout"))
  )
}

tree_viz_server <- function(id, rv, ai_card_fn) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # --- AI Panel Toggle ---
    observeEvent(input$toggle_ai_panel, {
      rv$show_ai_panel <- !rv$show_ai_panel
      updateActionButton(
        session, "toggle_ai_panel",
        label = if (rv$show_ai_panel) "Hide AI Chat" else "Show AI Chat"
      )
    })

    # --- Dynamic Layout ---
    output$tree_layout <- renderUI({
      if (rv$show_ai_panel) {
        layout_columns(
          col_widths = c(7, 5),
          tree_card_ui(),
          ai_card_fn()
        )
      } else {
        tree_card_ui()
      }
    })

    # Helper: tree card UI
    tree_card_ui <- function() {
      card(
        card_header(
          class = "d-flex justify-content-between align-items-center",
          span("Decision Tree Visualization"),
          conditionalPanel(
            condition = "output.model_built",
            downloadButton(
              ns("export_pptx"),
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
            plotOutput(ns("tree_plot"), height = if (rv$show_ai_panel) "550px" else "650px")
          )
        )
      )
    }

    # --- Tree Plot ---
    output$tree_plot <- renderPlot({
      req(rv$model)

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
        main = paste("Decision Tree:", rv$outcome_var)
      )
    }, res = 96)

    # --- Export Handler ---
    output$export_pptx <- create_export_handler(rv)
  })
}
