# mod_factor_editor.R
# Sidebar panel for reordering factor levels

factor_editor_ui <- function(id) {
  ns <- NS(id)

  tagList(
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
        ns("factor_var_select"),
        "Factor Variable",
        choices = NULL
      ),

      uiOutput(ns("factor_levels_display")),

      selectInput(
        ns("factor_level_to_move"),
        NULL,
        choices = NULL,
        width = "100%"
      ),

      div(
        class = "d-flex gap-1 mt-1",
        actionButton(ns("factor_move_up"), NULL, icon = icon("chevron-up"),
                      class = "btn-sm btn-outline-secondary flex-fill"),
        actionButton(ns("factor_move_down"), NULL, icon = icon("chevron-down"),
                      class = "btn-sm btn-outline-secondary flex-fill"),
        actionButton(ns("factor_set_ref"), "Set as First",
                      class = "btn-sm btn-outline-secondary flex-fill")
      ),

      actionButton(ns("factor_apply"), "Apply Order",
                    class = "btn-primary btn-sm w-100 mt-2",
                    icon = icon("check"))
    )
  )
}

factor_editor_server <- function(id, rv) {
  moduleServer(id, function(input, output, session) {

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
  })
}
