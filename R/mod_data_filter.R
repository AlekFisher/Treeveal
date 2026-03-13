# mod_data_filter.R
# Sidebar panel for applying a single active filter to the dataset.

data_filter_ui <- function(id) {
  ns <- NS(id)

  tagList(
    conditionalPanel(
      condition = "!output.data_loaded",
      helpText(class = "small text-muted", "Upload data to enable row-level filtering")
    ),

    conditionalPanel(
      condition = "output.data_loaded",
      helpText(
        class = "small text-muted mb-2",
        "Filter the dataset by one variable at a time. The filter applies across the whole app."
      ),

      selectInput(
        ns("filter_var"),
        "Filter Variable",
        choices = c("None" = ""),
        selected = ""
      ),

      uiOutput(ns("filter_value_ui")),

      div(
        class = "d-flex gap-2 mt-2",
        actionButton(
          ns("apply_filter"),
          "Apply Filter",
          class = "btn-primary btn-sm flex-fill",
          icon = icon("filter")
        ),
        actionButton(
          ns("clear_filter"),
          "Clear",
          class = "btn-outline-secondary btn-sm flex-fill",
          icon = icon("times")
        )
      ),

      div(
        class = "mt-2",
        uiOutput(ns("filter_summary"))
      )
    )
  )
}

data_filter_server <- function(id, rv) {
  moduleServer(id, function(input, output, session) {

    filterable_vars <- reactive({
      get_filterable_variables(rv$data_raw)
    })

    observe({
      choices <- c("None" = "", filterable_vars())
      selected <- input$filter_var

      if (is.null(selected) || !selected %in% unname(choices)) {
        selected <- ""
      }

      updateSelectInput(session, "filter_var", choices = choices, selected = selected)
    })

    observeEvent(rv$data_raw, {
      valid_vars <- filterable_vars()
      active_filter <- rv$active_filter

      if (!is.null(active_filter) && !active_filter$variable %in% valid_vars) {
        rv$active_filter <- NULL
      }
    })

    output$filter_value_ui <- renderUI({
      req(rv$data_raw)

      variable <- input$filter_var
      if (is.null(variable) || variable == "" || !variable %in% names(rv$data_raw)) {
        return(NULL)
      }

      column <- rv$data_raw[[variable]]

      if (is.numeric(column)) {
        rng <- range(column, na.rm = TRUE)
        active_filter <- rv$active_filter
        active_range <- if (!is.null(active_filter) &&
          identical(active_filter$variable, variable) &&
          identical(active_filter$type, "numeric")) {
          active_filter$range
        } else {
          rng
        }

        sliderInput(
          session$ns("filter_range"),
          "Value Range",
          min = rng[1],
          max = rng[2],
          value = active_range,
          step = signif((rng[2] - rng[1]) / 100, 3)
        )
      } else {
        values <- sort(unique(as.character(stats::na.omit(column))))
        active_filter <- rv$active_filter
        selected_values <- if (!is.null(active_filter) &&
          identical(active_filter$variable, variable) &&
          identical(active_filter$type, "categorical")) {
          intersect(active_filter$values, values)
        } else {
          values
        }

        selectInput(
          session$ns("filter_values"),
          "Allowed Values",
          choices = values,
          selected = selected_values,
          multiple = TRUE
        )
      }
    })

    candidate_filter <- reactive({
      req(rv$data_raw)

      variable <- input$filter_var
      if (is.null(variable) || variable == "" || !variable %in% names(rv$data_raw)) {
        return(NULL)
      }

      column <- rv$data_raw[[variable]]

      if (is.numeric(column)) {
        build_single_filter_spec(
          data = rv$data_raw,
          variable = variable,
          range = input$filter_range
        )
      } else {
        build_single_filter_spec(
          data = rv$data_raw,
          variable = variable,
          selected_values = input$filter_values
        )
      }
    })

    observeEvent(input$apply_filter, {
      req(rv$data_raw)

      filter_spec <- candidate_filter()
      if (is.null(filter_spec)) {
        showNotification("Choose a variable and valid filter values first.", type = "warning")
        return()
      }

      filtered_data <- apply_single_filter(rv$data_raw, filter_spec)
      if (nrow(filtered_data) == 0) {
        showNotification("That filter would remove all rows. Please widen the selection.", type = "warning")
        return()
      }

      rv$active_filter <- filter_spec
      rv$model <- NULL
      rv$chat <- NULL
      rv$chat_history <- list()

      showNotification(
        paste0("Filter applied: ", nrow(filtered_data), " of ", nrow(rv$data_raw), " rows kept."),
        type = "message"
      )
    })

    observeEvent(input$clear_filter, {
      rv$active_filter <- NULL
      updateSelectInput(session, "filter_var", selected = "")
      rv$model <- NULL
      rv$chat <- NULL
      rv$chat_history <- list()
      showNotification("Filter cleared.", type = "message")
    })

    output$filter_summary <- renderUI({
      req(rv$data_raw)

      active_filter <- rv$active_filter
      if (is.null(active_filter)) {
        return(
          p(class = "small text-muted mb-0", "No active filter. All rows are included.")
        )
      }

      kept_rows <- if (is.null(rv$data)) 0 else nrow(rv$data)
      total_rows <- nrow(rv$data_raw)

      div(
        class = "small",
        p(class = "mb-1", tags$strong("Active Filter")),
        p(class = "mb-1 text-muted", format_filter_summary(active_filter)),
        p(class = "mb-0 text-muted", paste0("Rows kept: ", format(kept_rows, big.mark = ","), " / ", format(total_rows, big.mark = ",")))
      )
    })
  })
}
