# mod_bivariate.R
# Bivariate Analysis tab: single summary table for top predictors

bivariate_ui <- function(id) {
  ns <- NS(id)

  navset_card_tab(
    id = ns("bivariate_tabs"),

    nav_panel(
      title = "Statistical Summary",
      icon = bsicons::bs_icon("calculator"),
      conditionalPanel(
        condition = "output.data_loaded",
        div(class = "text-muted small mb-3", "Top 20 predictors ranked by model importance with statistical tests."),
        DT::dataTableOutput(ns("biv_table"))
      ),
      conditionalPanel(
        condition = "!output.data_loaded",
        div(class = "text-center py-5 text-muted", "Load data to view bivariate analysis")
      )
    ),

    nav_panel(
      title = "Distribution Table",
      icon = bsicons::bs_icon("table"),
      conditionalPanel(
        condition = "output.data_loaded",
        div(class = "text-muted small mb-3", "Cross-tabulation showing how predictors distribute across outcome levels."),
        gt::gt_output(ns("distribution_table"))
      ),
      conditionalPanel(
        condition = "!output.data_loaded",
        div(class = "text-center py-5 text-muted", "Load data to view distributions")
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

    # Reactive list of top predictors
    top_predictors <- reactive({
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

    # Build summary statistics for one predictor
    compute_bivariate_stats <- function(predictor, outcome_var, data) {
      tryCatch({
        outcome <- outcome_var

        is_outcome_cat <- is.factor(data[[outcome]]) || is.character(data[[outcome]])
        is_predictor_cat <- is.factor(data[[predictor]]) || is.character(data[[predictor]])

        df <- data[!is.na(data[[outcome]]) & !is.na(data[[predictor]]), ]
        if (nrow(df) == 0) {
          return(data.frame(
            Predictor = predictor,
            Type = "Unknown",
            N = 0,
            Statistic = NA,
            stringsAsFactors = FALSE
          ))
        }

        pred_type <- if (is_predictor_cat) "Categorical" else "Numeric"

        if (is_outcome_cat && is_predictor_cat) {
          # Chi-square test for categorical vs categorical
          tbl <- table(df[[predictor]], df[[outcome]])
          chi_result <- tryCatch({
            chisq.test(tbl)
          }, error = function(e) NULL, warning = function(w) NULL)

          stat_str <- if (!is.null(chi_result)) {
            sprintf("χ² = %.2f (p = %.4f)", chi_result$statistic, chi_result$p.value)
          } else {
            "χ² test failed"
          }

          # Get distribution summary
          props <- prop.table(tbl, 1)
          outcome_levels <- colnames(tbl)
          if (length(outcome_levels) > 0) {
            # For each outcome level, show range of proportions across predictor levels
            prop_ranges <- sapply(1:ncol(props), function(i) {
              sprintf("%s: %.0f%%-%.0f%%",
                      outcome_levels[i],
                      min(props[,i]) * 100,
                      max(props[,i]) * 100)
            })
            dist_summary <- paste(prop_ranges, collapse = " | ")
          } else {
            dist_summary <- ""
          }

          data.frame(
            Predictor = predictor,
            Type = pred_type,
            N = nrow(df),
            Levels = length(unique(df[[predictor]])),
            Distribution = dist_summary,
            Statistic = stat_str,
            stringsAsFactors = FALSE
          )

        } else if (is_outcome_cat && !is_predictor_cat) {
          # ANOVA/t-test for numeric predictor vs categorical outcome
          formula_obj <- as.formula(paste(predictor, "~", outcome))
          anova_result <- tryCatch({
            aov_obj <- aov(formula_obj, data = df)
            summary(aov_obj)[[1]]
          }, error = function(e) NULL)

          stat_str <- if (!is.null(anova_result)) {
            f_val <- anova_result$`F value`[1]
            p_val <- anova_result$`Pr(>F)`[1]
            sprintf("F = %.2f (p = %.4f)", f_val, p_val)
          } else {
            "ANOVA failed"
          }

          # Get mean by outcome level
          means_by_outcome <- df %>%
            group_by(!!sym(outcome)) %>%
            summarize(Mean = mean(!!sym(predictor), na.rm = TRUE), .groups = "drop")

          mean_summary <- paste(
            sprintf("%s: %.2f", means_by_outcome[[outcome]], means_by_outcome$Mean),
            collapse = " | "
          )

          data.frame(
            Predictor = predictor,
            Type = pred_type,
            N = nrow(df),
            Levels = NA,
            Distribution = mean_summary,
            Statistic = stat_str,
            stringsAsFactors = FALSE
          )

        } else if (!is_outcome_cat && is_predictor_cat) {
          # ANOVA for categorical predictor vs numeric outcome
          formula_obj <- as.formula(paste(outcome, "~", predictor))
          anova_result <- tryCatch({
            aov_obj <- aov(formula_obj, data = df)
            summary(aov_obj)[[1]]
          }, error = function(e) NULL)

          stat_str <- if (!is.null(anova_result)) {
            f_val <- anova_result$`F value`[1]
            p_val <- anova_result$`Pr(>F)`[1]
            sprintf("F = %.2f (p = %.4f)", f_val, p_val)
          } else {
            "ANOVA failed"
          }

          # Get mean outcome by predictor level
          means_by_pred <- df %>%
            group_by(!!sym(predictor)) %>%
            summarize(Mean = mean(!!sym(outcome), na.rm = TRUE), .groups = "drop")

          mean_summary <- paste(
            sprintf("%s: %.2f", means_by_pred[[predictor]], means_by_pred$Mean),
            collapse = " | "
          )

          data.frame(
            Predictor = predictor,
            Type = pred_type,
            N = nrow(df),
            Levels = length(unique(df[[predictor]])),
            Distribution = mean_summary,
            Statistic = stat_str,
            stringsAsFactors = FALSE
          )

        } else {
          # Correlation for numeric vs numeric
          cor_result <- tryCatch({
            cor.test(df[[predictor]], df[[outcome]], method = "pearson")
          }, error = function(e) NULL)

          stat_str <- if (!is.null(cor_result)) {
            sprintf("r = %.3f (p = %.4f)", cor_result$estimate, cor_result$p.value)
          } else {
            "Correlation failed"
          }

          # Binned means
          df_binned <- df %>%
            mutate(
              Bin = cut(!!sym(predictor),
                       breaks = unique(quantile(!!sym(predictor), probs = seq(0, 1, by = 0.25), na.rm = TRUE)),
                       include.lowest = TRUE)
            )

          means_by_bin <- df_binned %>%
            group_by(Bin) %>%
            summarize(Mean = mean(!!sym(outcome), na.rm = TRUE), .groups = "drop")

          mean_summary <- paste(
            sprintf("%s: %.2f", means_by_bin$Bin, means_by_bin$Mean),
            collapse = " | "
          )

          data.frame(
            Predictor = predictor,
            Type = pred_type,
            N = nrow(df),
            Levels = NA,
            Distribution = mean_summary,
            Statistic = stat_str,
            stringsAsFactors = FALSE
          )
        }
      }, error = function(e) {
        data.frame(
          Predictor = predictor,
          Type = "Error",
          N = 0,
          Levels = NA,
          Distribution = e$message,
          Statistic = NA,
          stringsAsFactors = FALSE
        )
      })
    }

    # Main table output
    output$biv_table <- DT::renderDataTable({
      req(tab_seen(), rv$data, rv$outcome_var)
      preds <- top_predictors()
      req(length(preds) > 0)

      # Build summary for all predictors
      summary_list <- lapply(preds, function(p) {
        compute_bivariate_stats(p, rv$outcome_var, rv$data)
      })

      summary_df <- bind_rows(summary_list)

      # Add importance if available
      if (!is.null(rv$model) && !is.null(rv$model$variable.importance)) {
        imp_df <- data.frame(
          Predictor = names(rv$model$variable.importance),
          Importance = as.numeric(rv$model$variable.importance),
          stringsAsFactors = FALSE
        )
        summary_df <- summary_df %>%
          left_join(imp_df, by = "Predictor") %>%
          relocate(Importance, .after = Predictor)
      }

      # Render with DT
      dt_obj <- DT::datatable(
        summary_df,
        options = list(
          pageLength = 20,
          dom = 'ft',
          ordering = FALSE,
          columnDefs = list(
            list(className = 'dt-left', targets = 0),
            list(className = 'dt-center', targets = c(1, 2, 3)),
            list(className = 'dt-left', targets = '_all')
          )
        ),
        rownames = FALSE,
        class = 'cell-border stripe hover',
        style = 'bootstrap5'
      )

      # Format numeric columns if they exist
      if ("Importance" %in% names(summary_df)) {
        dt_obj <- dt_obj %>%
          DT::formatRound(columns = "Importance", digits = 2) %>%
          DT::formatStyle(
            'Importance',
            background = DT::styleColorBar(range(summary_df$Importance, na.rm = TRUE), THEME_ACCENT),
            backgroundSize = '95% 80%',
            backgroundRepeat = 'no-repeat',
            backgroundPosition = 'right center'
          )
      }

      if ("N" %in% names(summary_df)) {
        dt_obj <- dt_obj %>%
          DT::formatRound(columns = "N", digits = 0)
      }

      if ("Levels" %in% names(summary_df)) {
        dt_obj <- dt_obj %>%
          DT::formatRound(columns = "Levels", digits = 0)
      }

      dt_obj
    })

    # Distribution summary table using gt
    output$distribution_table <- gt::render_gt({
      req(tab_seen(), rv$data, rv$outcome_var)
      preds <- top_predictors()
      req(length(preds) > 0)

      outcome <- rv$outcome_var
      is_outcome_cat <- is.factor(rv$data[[outcome]]) || is.character(rv$data[[outcome]])

      # Build summary for all predictors
      summary_list <- lapply(preds, function(predictor) {
        tryCatch({
          is_predictor_cat <- is.factor(rv$data[[predictor]]) || is.character(rv$data[[predictor]])
          df <- rv$data[!is.na(rv$data[[outcome]]) & !is.na(rv$data[[predictor]]), ]

          if (nrow(df) == 0) return(NULL)

          if (is_outcome_cat) {
            # Outcome is categorical - show stats by outcome level
            outcome_levels <- sort(unique(df[[outcome]]))

            if (is_predictor_cat) {
              # For categorical predictors, create a row for each category level
              predictor_levels <- sort(unique(df[[predictor]]))

              result_list <- lapply(predictor_levels, function(pred_level) {
                row_data <- data.frame(
                  Predictor = paste0(predictor, " = ", pred_level),
                  Type = "Categorical",
                  stringsAsFactors = FALSE
                )

                # Calculate percentage for each outcome level
                for (out_level in outcome_levels) {
                  # Count of this predictor level within this outcome level
                  n_pred_and_out <- sum(df[[predictor]] == pred_level & df[[outcome]] == out_level, na.rm = TRUE)
                  # Total count for this outcome level
                  n_out <- sum(df[[outcome]] == out_level, na.rm = TRUE)
                  # Percentage
                  pct <- if (n_out > 0) (n_pred_and_out / n_out * 100) else 0

                  row_data[[as.character(out_level)]] <- sprintf("%.1f%%", pct)
                }
                row_data
              })

              result <- bind_rows(result_list)
              result

            } else {
              # Show mean for each outcome level
              mean_by_outcome <- sapply(outcome_levels, function(lvl) {
                subset_data <- df[df[[outcome]] == lvl, predictor]
                mean(subset_data, na.rm = TRUE)
              })

              result <- data.frame(
                Predictor = predictor,
                Type = "Numeric",
                stringsAsFactors = FALSE
              )
              for (i in seq_along(outcome_levels)) {
                result[[as.character(outcome_levels[i])]] <- sprintf("%.2f", mean_by_outcome[i])
              }
              result
            }

          } else {
            # Outcome is numeric - show by quartiles
            quartiles <- quantile(df[[outcome]], probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE)
            df$outcome_quartile <- cut(df[[outcome]],
                                       breaks = quartiles,
                                       labels = c("Q1 (Low)", "Q2", "Q3", "Q4 (High)"),
                                       include.lowest = TRUE)

            if (is_predictor_cat) {
              # For categorical predictors, create a row for each category level
              predictor_levels <- sort(unique(df[[predictor]]))
              quartile_levels <- levels(df$outcome_quartile)

              result_list <- lapply(predictor_levels, function(pred_level) {
                row_data <- data.frame(
                  Predictor = paste0(predictor, " = ", pred_level),
                  Type = "Categorical",
                  stringsAsFactors = FALSE,
                  check.names = FALSE
                )

                # Calculate percentage for each quartile
                for (q in quartile_levels) {
                  # Count of this predictor level within this quartile
                  n_pred_and_q <- sum(df[[predictor]] == pred_level & df$outcome_quartile == q, na.rm = TRUE)
                  # Total count for this quartile
                  n_q <- sum(df$outcome_quartile == q, na.rm = TRUE)
                  # Percentage
                  pct <- if (n_q > 0) (n_pred_and_q / n_q * 100) else 0

                  row_data[[q]] <- sprintf("%.1f%%", pct)
                }
                row_data
              })

              result <- bind_rows(result_list)
              result

            } else {
              # Show mean for each quartile
              mean_by_quartile <- sapply(levels(df$outcome_quartile), function(q) {
                subset_data <- df[df$outcome_quartile == q, predictor]
                mean(subset_data, na.rm = TRUE)
              })

              result <- data.frame(
                Predictor = predictor,
                Type = "Numeric",
                `Q1 (Low)` = sprintf("%.2f", mean_by_quartile[1]),
                Q2 = sprintf("%.2f", mean_by_quartile[2]),
                Q3 = sprintf("%.2f", mean_by_quartile[3]),
                `Q4 (High)` = sprintf("%.2f", mean_by_quartile[4]),
                stringsAsFactors = FALSE,
                check.names = FALSE
              )
              result
            }
          }

        }, error = function(e) {
          NULL
        })
      })

      # Remove NULL entries and combine
      summary_list <- summary_list[!sapply(summary_list, is.null)]
      if (length(summary_list) == 0) return(NULL)

      summary_df <- bind_rows(summary_list)

      # Extract base predictor names for grouping (remove " = value" suffix)
      summary_df$BasePredictor <- sapply(summary_df$Predictor, function(x) {
        parts <- strsplit(x, " = ")[[1]]
        parts[1]
      })

      # Create gt table with row grouping
      outcome_cols <- setdiff(names(summary_df), c("Predictor", "Type", "BasePredictor"))

      gt_obj <- gt::gt(summary_df, groupname_col = "BasePredictor") %>%
        gt::tab_header(
          title = gt::md(sprintf("**Distribution by %s**", outcome)),
          subtitle = if (is_outcome_cat) {
            "Percentages show distribution within each outcome level | Numeric values show means"
          } else {
            "Percentages show distribution within each quartile | Numeric values show means"
          }
        ) %>%
        gt::tab_spanner(
          label = gt::md(sprintf("**%s**", outcome)),
          columns = gt::all_of(outcome_cols)
        ) %>%
        gt::cols_hide(columns = "BasePredictor") %>%
        gt::cols_label(
          Predictor = ""
        ) %>%
        gt::cols_align(
          align = "left",
          columns = "Predictor"
        ) %>%
        gt::cols_align(
          align = "center",
          columns = c("Type", gt::all_of(outcome_cols))
        ) %>%
        gt::tab_options(
          table.width = gt::pct(100),
          table.font.size = gt::px(14),
          table.border.top.style = "none",
          table.border.bottom.style = "solid",
          table.border.bottom.width = gt::px(2),
          table.border.bottom.color = THEME_BORDER,
          heading.align = "left",
          heading.title.font.size = gt::px(18),
          heading.title.font.weight = "bold",
          heading.subtitle.font.size = gt::px(13),
          heading.padding = gt::px(12),
          column_labels.font.weight = "600",
          column_labels.font.size = gt::px(13),
          column_labels.padding = gt::px(8),
          column_labels.border.top.style = "none",
          column_labels.border.bottom.width = gt::px(2),
          column_labels.border.bottom.color = THEME_TEXT_PRIMARY,
          column_labels.background.color = "#fafafa",
          data_row.padding = gt::px(8),
          row_group.font.weight = "bold",
          row_group.font.size = gt::px(14),
          row_group.background.color = "#f5f5f5",
          row_group.border.top.style = "solid",
          row_group.border.top.width = gt::px(2),
          row_group.border.top.color = THEME_BORDER,
          row_group.border.bottom.style = "solid",
          row_group.border.bottom.width = gt::px(1),
          row_group.border.bottom.color = THEME_BORDER_LIGHT,
          row_group.padding = gt::px(8)
        ) %>%
        gt::tab_style(
          style = gt::cell_text(color = THEME_TEXT_SECONDARY, size = gt::px(12)),
          locations = gt::cells_title(groups = "subtitle")
        ) %>%
        gt::tab_style(
          style = list(
            gt::cell_text(weight = "500", size = gt::px(13)),
            gt::cell_borders(sides = "bottom", color = THEME_BORDER_LIGHT, weight = gt::px(1))
          ),
          locations = gt::cells_body(columns = "Predictor")
        ) %>%
        gt::tab_style(
          style = gt::cell_text(size = gt::px(11), color = THEME_TEXT_SECONDARY),
          locations = gt::cells_body(columns = "Type")
        ) %>%
        gt::opt_table_font(font = list("Inter", "system-ui", "sans-serif")) %>%
        gt::opt_row_striping(row_striping = TRUE)

      # Add gradient coloring for percentage values in outcome columns
      for (col in outcome_cols) {
        # Extract numeric values from percentage strings for coloring
        col_values <- summary_df[[col]]
        numeric_values <- suppressWarnings(as.numeric(gsub("[^0-9.]", "", col_values)))

        if (any(!is.na(numeric_values))) {
          gt_obj <- gt_obj %>%
            gt::tab_style(
              style = gt::cell_fill(color = "#e8f4f8"),
              locations = gt::cells_body(
                columns = col,
                rows = !is.na(numeric_values) & numeric_values >= 50
              )
            ) %>%
            gt::tab_style(
              style = gt::cell_fill(color = "#fff3e0"),
              locations = gt::cells_body(
                columns = col,
                rows = !is.na(numeric_values) & numeric_values >= 25 & numeric_values < 50
              )
            ) %>%
            gt::tab_style(
              style = gt::cell_text(weight = "600"),
              locations = gt::cells_body(
                columns = col,
                rows = !is.na(numeric_values) & numeric_values >= 50
              )
            )
        }
      }

      gt_obj
    })

  })
}
