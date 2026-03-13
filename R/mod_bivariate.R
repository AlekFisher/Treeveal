# mod_bivariate.R
# Bivariate Analysis tab: summary statistics and proportions using gt

library(shiny)
library(bslib)
library(dplyr)
library(tidyr)
library(gt)
library(rlang)

bivariate_ui <- function(id) {
  ns <- NS(id)

  layout_columns(
    col_widths = c(12),

    card(
      card_header("Bivariate Summary"),
      card_body(
        conditionalPanel(
          condition = "output.data_loaded",
          div(class = "text-muted small mb-3", "Showing up to the top 20 predictors (sorted by model importance if available)."),
          uiOutput(ns("biv_tables_ui"))
        ),
        conditionalPanel(
          condition = "!output.data_loaded",
          div(class = "text-center py-5 text-muted", "Load data to view bivariate analysis")
        )
      )
    )
  )
}

bivariate_server <- function(id, rv, active_tab = reactive(NULL)) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Track whether this tab has been viewed at least once
    tab_seen <- reactiveVal(FALSE)
    observe({
      if (identical(active_tab(), "Bivariate Analysis")) tab_seen(TRUE)
    })

    # Reactive list of top predictors — debounced to avoid cascade
    top_predictors_raw <- reactive({
      req(rv$predictor_vars)
      preds <- rv$predictor_vars

      # If model exists, order by importance
      if (!is.null(rv$model) && !is.null(rv$model$variable.importance)) {
        imp <- names(rv$model$variable.importance)
        preds_in_tree <- intersect(imp, preds)
        preds_not_in_tree <- setdiff(preds, imp)
        ordered_preds <- c(preds_in_tree, preds_not_in_tree)
      } else {
        ordered_preds <- preds
      }

      # Take top 20
      head(ordered_preds, 20)
    })
    top_predictors <- top_predictors_raw %>% debounce(500)

    # Render the UI containers for the tables
    output$biv_tables_ui <- renderUI({
      req(tab_seen())
      preds <- top_predictors()
      req(length(preds) > 0)

      lapply(preds, function(p) {
        div(
          style = "margin-bottom: 24px; border-bottom: 1px solid #e5e5e5; padding-bottom: 24px;",
          gt_output(ns(paste0("table_", p)))
        )
      })
    })

    # Shared gt styling applied to every table
    style_gt <- function(gt_obj, n_rows) {
      styled <- gt_obj %>%
        tab_options(
          table.width                  = pct(100),
          table.font.size              = px(13),
          table.border.top.style       = "none",
          table.border.bottom.style    = "solid",
          table.border.bottom.width    = px(1),
          table.border.bottom.color    = THEME_BORDER,
          heading.align                = "left",
          heading.title.font.size      = px(14),
          heading.padding              = px(6),
          heading.border.bottom.style  = "none",
          column_labels.font.weight    = "bold",
          column_labels.font.size      = px(12),
          column_labels.padding        = px(6),
          column_labels.border.top.style    = "none",
          column_labels.border.bottom.width = px(2),
          column_labels.border.bottom.color = THEME_TEXT_PRIMARY,
          data_row.padding             = px(5),
          row_group.padding            = px(5),
          source_notes.font.size       = px(11),
          source_notes.padding         = px(4)
        ) %>%
        tab_style(
          style = cell_text(color = THEME_TEXT_SECONDARY, weight = "normal"),
          locations = cells_title(groups = "subtitle")
        ) %>%
        opt_table_font(font = list("Inter", "system-ui", "sans-serif"))

      # Only paginate if many rows
      if (n_rows > 15) {
        styled <- styled %>%
          opt_interactive(use_pagination = TRUE, pagination_type = "numbers")
      }

      styled
    }

    # Observe top predictors and generate tables (only after tab has been seen)
    observeEvent(list(rv$data, top_predictors()), {
      req(tab_seen(), rv$data, rv$outcome_var)
      preds <- top_predictors()

      for (p in preds) {
        local({
          predictor <- p
          output_id <- paste0("table_", predictor)

          output[[output_id]] <- render_gt({
            outcome <- rv$outcome_var

            is_outcome_cat <- is.factor(rv$data[[outcome]]) || is.character(rv$data[[outcome]])
            is_predictor_cat <- is.factor(rv$data[[predictor]]) || is.character(rv$data[[predictor]])

            df <- rv$data[!is.na(rv$data[[outcome]]) & !is.na(rv$data[[predictor]]), ]
            req(nrow(df) > 0)

            if (is_outcome_cat && is_predictor_cat) {
              # ── Both Categorical: merged count (pct) columns + total row ──
              tbl_data <- df %>%
                count(!!sym(predictor), !!sym(outcome)) %>%
                group_by(!!sym(predictor)) %>%
                mutate(
                  row_n = sum(n),
                  Pct   = n / row_n
                ) %>%
                ungroup()

              # Build merged "n (pct)" strings
              tbl_merged <- tbl_data %>%
                mutate(label = sprintf("%s (%s%%)", formatC(n, format = "d", big.mark = ","),
                                       formatC(Pct * 100, format = "f", digits = 1))) %>%
                select(!!sym(predictor), !!sym(outcome), label) %>%
                pivot_wider(names_from = !!sym(outcome), values_from = label, values_fill = "0 (0.0%)")

              # Row totals
              row_totals <- tbl_data %>%
                distinct(!!sym(predictor), row_n) %>%
                rename(N = row_n)

              res_tbl <- tbl_merged %>%
                left_join(row_totals, by = predictor) %>%
                relocate(N, .after = !!sym(predictor))

              # Total row
              total_data <- df %>%
                count(!!sym(outcome)) %>%
                mutate(Pct = n / sum(n)) %>%
                mutate(label = sprintf("%s (%s%%)", formatC(n, format = "d", big.mark = ","),
                                       formatC(Pct * 100, format = "f", digits = 1))) %>%
                select(!!sym(outcome), label) %>%
                pivot_wider(names_from = !!sym(outcome), values_from = label, values_fill = "0 (0.0%)")
              total_data[[predictor]] <- "Total"
              total_data[["N"]] <- nrow(df)
              total_data <- total_data %>% relocate(!!sym(predictor), N)

              res_tbl <- bind_rows(res_tbl, total_data)

              outcome_levels <- setdiff(names(res_tbl), c(predictor, "N"))

              gt_obj <- gt(res_tbl) %>%
                tab_header(
                  title    = md(sprintf("**%s** by %s", outcome, predictor)),
                  subtitle = "Count (row %)"
                ) %>%
                fmt_number(columns = "N", decimals = 0) %>%
                tab_spanner(
                  label   = outcome,
                  columns = all_of(outcome_levels)
                ) %>%
                tab_style(
                  style = list(
                    cell_borders(sides = "top", weight = px(2), color = THEME_TEXT_PRIMARY),
                    cell_text(weight = "bold")
                  ),
                  locations = cells_body(rows = nrow(res_tbl))
                ) %>%
                cols_align(align = "right", columns = c("N", all_of(outcome_levels))) %>%
                cols_align(align = "left", columns = !!sym(predictor))

              style_gt(gt_obj, nrow(res_tbl))

            } else if (is_outcome_cat && !is_predictor_cat) {
              # ── Categorical Outcome / Numeric Predictor ──
              res_tbl <- df %>%
                group_by(!!sym(outcome)) %>%
                summarize(
                  N      = n(),
                  Mean   = mean(!!sym(predictor), na.rm = TRUE),
                  SD     = sd(!!sym(predictor), na.rm = TRUE),
                  Min    = min(!!sym(predictor), na.rm = TRUE),
                  Median = median(!!sym(predictor), na.rm = TRUE),
                  Max    = max(!!sym(predictor), na.rm = TRUE),
                  .groups = "drop"
                )

              gt_obj <- gt(res_tbl) %>%
                tab_header(
                  title    = md(sprintf("**%s** by %s", predictor, outcome)),
                  subtitle = "Descriptive statistics"
                ) %>%
                fmt_number(columns = c("Mean", "SD", "Min", "Median", "Max"), decimals = 2) %>%
                fmt_number(columns = "N", decimals = 0) %>%
                data_color(
                  columns = "Mean",
                  method  = "numeric",
                  palette = c(THEME_BG_PRIMARY, THEME_ACCENT),
                  alpha   = 0.15
                ) %>%
                cols_align(align = "right", columns = -!!sym(outcome))

              style_gt(gt_obj, nrow(res_tbl))

            } else if (!is_outcome_cat && is_predictor_cat) {
              # ── Numeric Outcome / Categorical Predictor ──
              res_tbl <- df %>%
                group_by(!!sym(predictor)) %>%
                summarize(
                  N      = n(),
                  Mean   = mean(!!sym(outcome), na.rm = TRUE),
                  SD     = sd(!!sym(outcome), na.rm = TRUE),
                  Min    = min(!!sym(outcome), na.rm = TRUE),
                  Median = median(!!sym(outcome), na.rm = TRUE),
                  Max    = max(!!sym(outcome), na.rm = TRUE),
                  .groups = "drop"
                )

              gt_obj <- gt(res_tbl) %>%
                tab_header(
                  title    = md(sprintf("**%s** by %s", outcome, predictor)),
                  subtitle = "Descriptive statistics"
                ) %>%
                fmt_number(columns = c("Mean", "SD", "Min", "Median", "Max"), decimals = 2) %>%
                fmt_number(columns = "N", decimals = 0) %>%
                data_color(
                  columns = "Mean",
                  method  = "numeric",
                  palette = c(THEME_BG_PRIMARY, THEME_ACCENT),
                  alpha   = 0.15
                ) %>%
                cols_align(align = "right", columns = -!!sym(predictor))

              style_gt(gt_obj, nrow(res_tbl))

            } else {
              # ── Both Numeric: bin predictor ──
              df_binned <- df %>%
                mutate(
                  Predictor_Bin = if (length(unique(!!sym(predictor))) > 5) {
                    cut(!!sym(predictor), breaks = unique(quantile(!!sym(predictor), probs = seq(0, 1, by = 0.2), na.rm = TRUE)), include.lowest = TRUE)
                  } else {
                    as.factor(!!sym(predictor))
                  }
                )

              res_tbl <- df_binned %>%
                group_by(Predictor_Bin) %>%
                summarize(
                  N      = n(),
                  Mean   = mean(!!sym(outcome), na.rm = TRUE),
                  SD     = sd(!!sym(outcome), na.rm = TRUE),
                  Min    = min(!!sym(outcome), na.rm = TRUE),
                  Median = median(!!sym(outcome), na.rm = TRUE),
                  Max    = max(!!sym(outcome), na.rm = TRUE),
                  .groups = "drop"
                ) %>%
                rename(!!sym(predictor) := Predictor_Bin)

              gt_obj <- gt(res_tbl) %>%
                tab_header(
                  title    = md(sprintf("**%s** by %s (binned)", outcome, predictor)),
                  subtitle = "Descriptive statistics"
                ) %>%
                fmt_number(columns = c("Mean", "SD", "Min", "Median", "Max"), decimals = 2) %>%
                fmt_number(columns = "N", decimals = 0) %>%
                data_color(
                  columns = "Mean",
                  method  = "numeric",
                  palette = c(THEME_BG_PRIMARY, THEME_ACCENT),
                  alpha   = 0.15
                ) %>%
                cols_align(align = "right", columns = -!!sym(predictor))

              style_gt(gt_obj, nrow(res_tbl))
            }
          })
        })
      }
    })

  })
}
