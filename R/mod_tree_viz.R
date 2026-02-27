# mod_tree_viz.R
# Decision Tree tab: toggle button, dynamic tree+AI layout, static/interactive views

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
            div(
              class = "d-flex align-items-center gap-2",
              div(
                class = "tree-view-toggle",
                radioButtons(
                  ns("view_mode"),
                  label = NULL,
                  choices = c("Static" = "static", "Interactive" = "interactive"),
                  selected = "static",
                  inline = TRUE
                )
              ),
              downloadButton(
                ns("export_pptx"),
                "Export",
                class = "btn-sm btn-outline-primary",
                icon = icon("file-powerpoint")
              )
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
            uiOutput(ns("active_tree_view"))
          )
        )
      )
    }

    # --- Active View Switcher ---
    output$active_tree_view <- renderUI({
      req(rv$model)
      view <- input$view_mode %||% "static"
      plot_height <- if (rv$show_ai_panel) "550px" else "650px"

      if (view == "interactive") {
        visNetwork::visNetworkOutput(ns("tree_interactive"), height = plot_height)
      } else {
        plotOutput(ns("tree_plot"), height = plot_height)
      }
    })

    # --- Static Tree Plot ---
    output$tree_plot <- renderPlot({
      req(rv$model)
      render_tree_plot(rv$model, title = paste("Decision Tree:", rv$outcome_var))
    }, res = 120)

    # --- Interactive Tree (visNetwork) ---
    output$tree_interactive <- visNetwork::renderVisNetwork({
      req(rv$model)
      vn_data <- rpart_to_visnetwork(rv$model, rv$data_dict)

      visNetwork::visNetwork(
        vn_data$nodes,
        vn_data$edges,
        main = list(text = paste("Decision Tree:", rv$outcome_var),
                    style = "font-family: Inter, sans-serif; font-size: 14px; font-weight: 500; color: #737373;")
      ) |>
        visNetwork::visHierarchicalLayout(
          direction = "UD",
          sortMethod = "directed",
          levelSeparation = 120,
          nodeSpacing = 150
        ) |>
        visNetwork::visNodes(
          font = list(size = 14, face = "Inter, sans-serif"),
          borderWidth = 1,
          shadow = FALSE
        ) |>
        visNetwork::visEdges(
          arrows = "to",
          color = list(color = "#d4d4d4", highlight = "#2563eb"),
          font = list(size = 11, color = "#737373", face = "Inter, sans-serif"),
          smooth = list(type = "cubicBezier")
        ) |>
        visNetwork::visInteraction(
          hover = TRUE,
          tooltipDelay = 100,
          zoomView = TRUE,
          dragView = TRUE,
          navigationButtons = TRUE
        ) |>
        visNetwork::visPhysics(enabled = FALSE) |>
        visNetwork::visOptions(
          collapse = list(enabled = TRUE)
        )
    })

    # --- Export Handler ---
    output$export_pptx <- create_export_handler(rv)
  })
}
