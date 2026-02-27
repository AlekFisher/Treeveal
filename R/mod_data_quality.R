# mod_data_quality.R
# Data Quality tab: summary, variable checks, recommendations

data_quality_ui <- function(id) {
  ns <- NS(id)

  tagList(
    conditionalPanel(
      condition = "!output.data_loaded",
      card(
        card_body(
          div(
            class = "text-center py-5",
            bsicons::bs_icon("clipboard-check", size = "4rem", class = "text-muted"),
            h4(class = "text-muted mt-3", "Upload data to see quality checks"),
            p(class = "text-muted", "Data validation will appear here after you upload a dataset")
          )
        )
      )
    ),

    conditionalPanel(
      condition = "output.data_loaded",

      layout_columns(
        col_widths = c(12),

        # Summary card
        card(
          card_body(
            class = "p-3",
            layout_columns(
              col_widths = c(3, 3, 3, 3),
              value_box(
                title = "Ready for Analysis",
                value = textOutput(ns("dq_ready_vars"), inline = TRUE),
                showcase = bsicons::bs_icon("check-circle"),
                showcase_layout = "left center",
                theme = "success"
              ),
              value_box(
                title = "Warnings",
                value = textOutput(ns("dq_warning_vars"), inline = TRUE),
                showcase = bsicons::bs_icon("exclamation-triangle"),
                showcase_layout = "left center",
                theme = "warning"
              ),
              value_box(
                title = "Issues",
                value = textOutput(ns("dq_issue_vars"), inline = TRUE),
                showcase = bsicons::bs_icon("x-circle"),
                showcase_layout = "left center",
                theme = "danger"
              ),
              value_box(
                title = "Overall Status",
                value = textOutput(ns("dq_overall_status"), inline = TRUE),
                showcase = bsicons::bs_icon("speedometer2"),
                showcase_layout = "left center",
                theme = "info"
              )
            )
          )
        )
      ),

      layout_columns(
        col_widths = c(6, 6),

        # Variable Quality Table
        card(
          card_header("Variable Quality Summary"),
          card_body(
            p(class = "text-muted small", "Click a row to see details. Variables are flagged based on missingness, variance, and data type."),
            DTOutput(ns("dq_variable_table"))
          )
        ),

        # Issue Details
        card(
          card_header("Quality Check Details"),
          card_body(
            accordion(
              id = ns("dq_accordion"),
              open = FALSE,

              accordion_panel(
                title = "Missing Data",
                value = "missing",
                icon = bsicons::bs_icon("question-circle"),
                p(class = "small text-muted", "Variables with missing values. High missingness (>50%) may indicate skip logic or data collection issues."),
                DTOutput(ns("dq_missing_table"))
              ),

              accordion_panel(
                title = "Low Variance",
                value = "variance",
                icon = bsicons::bs_icon("dash-circle"),
                p(class = "small text-muted", "Variables with very low variance won't contribute meaningful splits. Consider excluding near-constant variables."),
                DTOutput(ns("dq_variance_table"))
              ),

              accordion_panel(
                title = "Outcome Variable Check",
                value = "outcome",
                icon = bsicons::bs_icon("bullseye"),
                p(class = "small text-muted", "Assessment of your selected outcome variable for decision tree suitability."),
                uiOutput(ns("dq_outcome_check"))
              ),

              accordion_panel(
                title = "Sample Size",
                value = "sample",
                icon = bsicons::bs_icon("people"),
                p(class = "small text-muted", "Minimum recommended sample sizes for reliable decision tree analysis."),
                uiOutput(ns("dq_sample_check"))
              ),

              accordion_panel(
                title = "High Correlation",
                value = "correlation",
                icon = bsicons::bs_icon("link-45deg"),
                p(class = "small text-muted", "Highly correlated predictors. Common in attitudinal data - not necessarily a problem, but good to be aware of."),
                DTOutput(ns("dq_correlation_table"))
              ),

              accordion_panel(
                title = "Outliers",
                value = "outliers",
                icon = bsicons::bs_icon("exclamation-diamond"),
                p(class = "small text-muted",
                  "Numeric variables checked for outliers using the IQR method. ",
                  "Mild: beyond 1.5\u00d7 IQR. Extreme: beyond 3\u00d7 IQR. Informational only."),
                DTOutput(ns("dq_outlier_table"))
              )
            )
          )
        )
      ),

      # Recommendations
      layout_columns(
        col_widths = c(12),
        card(
          card_header(
            class = "d-flex align-items-center",
            bsicons::bs_icon("lightbulb", class = "me-2"),
            "Recommendations"
          ),
          card_body(
            uiOutput(ns("dq_recommendations"))
          )
        )
      )
    )
  )
}

data_quality_server <- function(id, rv) {
  moduleServer(id, function(input, output, session) {

    # Reactive to compute all data quality metrics
    data_quality <- reactive({
      req(rv$data)

      df <- rv$data
      n_obs <- nrow(df)
      n_vars <- ncol(df)

      # --- Missing Data Analysis ---
      missing_info <- data.frame(
        Variable = names(df),
        N_Missing = sapply(df, function(x) sum(is.na(x))),
        Pct_Missing = sapply(df, function(x) round(sum(is.na(x)) / length(x) * 100, 1)),
        stringsAsFactors = FALSE
      )
      missing_info$Status <- dplyr::case_when(
        missing_info$Pct_Missing == 0 ~ "OK",
        missing_info$Pct_Missing < 20 ~ "Low",
        missing_info$Pct_Missing < 50 ~ "Moderate",
        TRUE ~ "High"
      )

      # --- Variance Analysis ---
      variance_info <- data.frame(
        Variable = names(df),
        stringsAsFactors = FALSE
      )

      variance_info$Type <- sapply(df, function(x) {
        if (is.factor(x) || is.character(x)) "Categorical"
        else if (is.numeric(x)) "Numeric"
        else "Other"
      })

      variance_info$Unique_Values <- sapply(df, function(x) length(unique(na.omit(x))))

      variance_info$Variance_Status <- sapply(seq_len(ncol(df)), function(i) {
        x <- df[[i]]
        x <- na.omit(x)
        if (length(x) == 0) return("No Data")

        n_unique <- length(unique(x))

        if (n_unique == 1) return("Constant")

        if (is.numeric(x)) {
          most_common_pct <- max(table(x)) / length(x)
          if (most_common_pct > 0.95) return("Near-Constant")
        } else {
          most_common_pct <- max(table(x)) / length(x)
          if (most_common_pct > 0.95) return("Near-Constant")
        }

        return("OK")
      })

      # --- Correlation Analysis (numeric variables only) ---
      numeric_vars <- names(df)[sapply(df, is.numeric)]
      high_corr <- data.frame(Var1 = character(), Var2 = character(), Correlation = numeric(), stringsAsFactors = FALSE)

      if (length(numeric_vars) >= 2) {
        numeric_df <- df[, numeric_vars, drop = FALSE]
        cor_matrix <- tryCatch({
          cor(numeric_df, use = "pairwise.complete.obs")
        }, error = function(e) NULL)

        if (!is.null(cor_matrix)) {
          for (i in 1:(ncol(cor_matrix) - 1)) {
            for (j in (i + 1):ncol(cor_matrix)) {
              if (!is.na(cor_matrix[i, j]) && abs(cor_matrix[i, j]) > 0.8) {
                high_corr <- rbind(high_corr, data.frame(
                  Var1 = numeric_vars[i],
                  Var2 = numeric_vars[j],
                  Correlation = round(cor_matrix[i, j], 3),
                  stringsAsFactors = FALSE
                ))
              }
            }
          }
        }
      }

      # --- Outlier Analysis (IQR method, numeric variables only) ---
      outlier_info <- data.frame(
        Variable = character(), Q1 = numeric(), Q3 = numeric(), IQR = numeric(),
        N_Mild = integer(), Pct_Mild = numeric(),
        N_Extreme = integer(), Pct_Extreme = numeric(),
        Status = character(), stringsAsFactors = FALSE
      )

      for (v in numeric_vars) {
        x <- na.omit(df[[v]])
        if (length(x) < 4) next
        q1 <- quantile(x, 0.25)
        q3 <- quantile(x, 0.75)
        iqr_val <- q3 - q1
        if (iqr_val == 0) next

        extreme_low <- q1 - 3 * iqr_val
        extreme_high <- q3 + 3 * iqr_val
        mild_low <- q1 - 1.5 * iqr_val
        mild_high <- q3 + 1.5 * iqr_val

        is_extreme <- x < extreme_low | x > extreme_high
        is_mild <- (x < mild_low | x > mild_high) & !is_extreme

        n_total <- length(x)
        n_extreme <- sum(is_extreme)
        n_mild <- sum(is_mild)

        status <- if (n_extreme / n_total > 0.05) "Extreme"
                  else if ((n_mild + n_extreme) / n_total > 0.05) "Mild"
                  else "OK"

        outlier_info <- rbind(outlier_info, data.frame(
          Variable = v,
          Q1 = round(q1, 2),
          Q3 = round(q3, 2),
          IQR = round(iqr_val, 2),
          N_Mild = n_mild,
          Pct_Mild = round(n_mild / n_total * 100, 1),
          N_Extreme = n_extreme,
          Pct_Extreme = round(n_extreme / n_total * 100, 1),
          Status = status,
          stringsAsFactors = FALSE
        ))
      }

      # --- Variable Summary ---
      var_summary <- merge(missing_info, variance_info, by = "Variable")
      # Left-join outlier Pct_Extreme into var_summary
      if (nrow(outlier_info) > 0) {
        outlier_match <- match(var_summary$Variable, outlier_info$Variable)
        var_summary$Pct_Extreme <- ifelse(
          is.na(outlier_match), 0, outlier_info$Pct_Extreme[outlier_match]
        )
      } else {
        var_summary$Pct_Extreme <- 0
      }

      var_summary$Overall_Status <- dplyr::case_when(
        var_summary$Variance_Status == "Constant" ~ "Issue",
        var_summary$Variance_Status == "No Data" ~ "Issue",
        var_summary$Pct_Missing > 50 ~ "Warning",
        var_summary$Variance_Status == "Near-Constant" ~ "Warning",
        var_summary$Pct_Missing > 20 ~ "Warning",
        var_summary$Pct_Extreme > 5 ~ "Warning",
        TRUE ~ "OK"
      )

      # Add labels from dictionary if available
      if (!is.null(rv$data_dict)) {
        var_summary$Label <- sapply(var_summary$Variable, function(v) {
          match_idx <- match(v, rv$data_dict$variable)
          if (!is.na(match_idx)) rv$data_dict$label[match_idx] else ""
        })
      } else {
        var_summary$Label <- ""
      }

      # --- Counts ---
      n_ok <- sum(var_summary$Overall_Status == "OK")
      n_warning <- sum(var_summary$Overall_Status == "Warning")
      n_issue <- sum(var_summary$Overall_Status == "Issue")

      list(
        missing = missing_info,
        variance = variance_info,
        correlation = high_corr,
        outliers = outlier_info,
        summary = var_summary,
        n_ok = n_ok,
        n_warning = n_warning,
        n_issue = n_issue,
        n_obs = n_obs,
        n_vars = n_vars
      )
    })

    # Summary value boxes
    output$dq_ready_vars <- renderText({
      req(data_quality())
      data_quality()$n_ok
    })

    output$dq_warning_vars <- renderText({
      req(data_quality())
      data_quality()$n_warning
    })

    output$dq_issue_vars <- renderText({
      req(data_quality())
      data_quality()$n_issue
    })

    output$dq_overall_status <- renderText({
      req(data_quality())
      dq <- data_quality()
      if (dq$n_issue > 0) {
        "Review Needed"
      } else if (dq$n_warning > 3) {
        "Some Concerns"
      } else {
        "Good"
      }
    })

    # Variable quality table
    output$dq_variable_table <- renderDT({
      req(data_quality())

      df <- data_quality()$summary
      df <- df[, c("Variable", "Label", "Type", "Pct_Missing", "Unique_Values", "Overall_Status")]
      names(df) <- c("Variable", "Label", "Type", "% Missing", "Unique", "Status")

      datatable(
        df,
        options = list(
          pageLength = 15,
          dom = 'frtip',
          scrollX = TRUE,
          order = list(list(5, 'desc'))
        ),
        class = 'compact stripe hover',
        selection = 'single'
      ) |>
        formatStyle(
          'Status',
          backgroundColor = styleEqual(
            c('OK', 'Warning', 'Issue'),
            c(THEME_STATUS_OK, THEME_STATUS_WARNING, THEME_STATUS_DANGER)
          ),
          fontWeight = 'bold'
        )
    })

    # Missing data table
    output$dq_missing_table <- renderDT({
      req(data_quality())

      df <- data_quality()$missing
      df <- df[df$Pct_Missing > 0, ]
      df <- df[order(-df$Pct_Missing), ]

      if (nrow(df) == 0) {
        return(datatable(data.frame(Message = "No missing data found"), options = list(dom = 't')))
      }

      names(df) <- c("Variable", "N Missing", "% Missing", "Status")

      datatable(
        df,
        options = list(pageLength = 10, dom = 'tip'),
        class = 'compact stripe'
      ) |>
        formatStyle(
          'Status',
          backgroundColor = styleEqual(
            c('OK', 'Low', 'Moderate', 'High'),
            c(THEME_STATUS_OK, THEME_STATUS_OK, THEME_STATUS_WARNING, THEME_STATUS_DANGER)
          )
        )
    })

    # Low variance table
    output$dq_variance_table <- renderDT({
      req(data_quality())

      df <- data_quality()$variance
      df <- df[df$Variance_Status != "OK", ]

      if (nrow(df) == 0) {
        return(datatable(data.frame(Message = "All variables have adequate variance"), options = list(dom = 't')))
      }

      names(df) <- c("Variable", "Type", "Unique Values", "Status")

      datatable(
        df,
        options = list(pageLength = 10, dom = 'tip'),
        class = 'compact stripe'
      ) |>
        formatStyle(
          'Status',
          backgroundColor = styleEqual(
            c('OK', 'Near-Constant', 'Constant', 'No Data'),
            c(THEME_STATUS_OK, THEME_STATUS_WARNING, THEME_STATUS_DANGER, THEME_STATUS_DANGER)
          )
        )
    })

    # Correlation table
    output$dq_correlation_table <- renderDT({
      req(data_quality())

      df <- data_quality()$correlation

      if (nrow(df) == 0) {
        return(datatable(data.frame(Message = "No highly correlated pairs found (r > 0.8)"), options = list(dom = 't')))
      }

      df <- df[order(-abs(df$Correlation)), ]

      datatable(
        df,
        options = list(pageLength = 10, dom = 'tip'),
        class = 'compact stripe'
      )
    })

    # Outlier table
    output$dq_outlier_table <- renderDT({
      req(data_quality())

      df <- data_quality()$outliers

      if (nrow(df) == 0 || all(df$Status == "OK")) {
        return(datatable(
          data.frame(Message = "No outliers detected"),
          options = list(dom = 't'),
          rownames = FALSE
        ))
      }

      df <- df[df$Status != "OK", ]
      df <- df[order(-df$Pct_Extreme), ]

      display_df <- data.frame(
        Variable = df$Variable,
        `Mild (n)` = df$N_Mild,
        `Mild (%)` = df$Pct_Mild,
        `Extreme (n)` = df$N_Extreme,
        `Extreme (%)` = df$Pct_Extreme,
        Severity = df$Status,
        check.names = FALSE,
        stringsAsFactors = FALSE
      )

      datatable(
        display_df,
        options = list(pageLength = 10, dom = 'tip'),
        class = 'compact stripe',
        rownames = FALSE
      ) |>
        formatStyle(
          'Severity',
          backgroundColor = styleEqual(
            c('Mild', 'Extreme'),
            c(THEME_STATUS_WARNING, THEME_STATUS_DANGER)
          ),
          fontWeight = 'bold'
        )
    })

    # Outcome variable check
    output$dq_outcome_check <- renderUI({
      req(rv$data, rv$outcome_var)

      outcome <- rv$data[[rv$outcome_var]]
      n_obs <- length(outcome)
      n_missing <- sum(is.na(outcome))

      checks <- list()

      if (is.factor(outcome) || is.character(outcome)) {
        # Classification
        levels_tbl <- table(outcome)
        n_levels <- length(levels_tbl)
        min_level_n <- min(levels_tbl)
        min_level_pct <- round(min(levels_tbl) / sum(levels_tbl) * 100, 1)

        checks$type <- list(
          status = "OK",
          text = paste0("Classification outcome with ", n_levels, " levels")
        )

        if (n_levels < 2) {
          checks$levels <- list(status = "Issue", text = "Only 1 level - cannot build classifier")
        } else if (n_levels > 10) {
          checks$levels <- list(status = "Warning", text = paste0(n_levels, " levels - consider grouping for interpretability"))
        } else {
          checks$levels <- list(status = "OK", text = paste0(n_levels, " levels"))
        }

        if (min_level_pct < 5) {
          checks$balance <- list(status = "Warning", text = paste0("Smallest class has only ", min_level_pct, "% of cases (n=", min_level_n, ")"))
        } else if (min_level_pct < 10) {
          checks$balance <- list(status = "OK", text = paste0("Minor imbalance - smallest class: ", min_level_pct, "%"))
        } else {
          checks$balance <- list(status = "OK", text = paste0("Balanced classes - smallest: ", min_level_pct, "%"))
        }

      } else if (is.numeric(outcome)) {
        # Regression
        checks$type <- list(status = "OK", text = "Regression outcome (continuous)")

        n_unique <- length(unique(na.omit(outcome)))
        if (n_unique < 5) {
          checks$levels <- list(status = "Warning", text = paste0("Only ", n_unique, " unique values - consider treating as categorical"))
        } else {
          checks$levels <- list(status = "OK", text = paste0(n_unique, " unique values"))
        }

        checks$balance <- list(status = "OK", text = "N/A for regression")
      }

      # Missing check
      if (n_missing > 0) {
        pct_missing <- round(n_missing / n_obs * 100, 1)
        if (pct_missing > 20) {
          checks$missing <- list(status = "Warning", text = paste0(pct_missing, "% missing values in outcome"))
        } else {
          checks$missing <- list(status = "OK", text = paste0(pct_missing, "% missing (will be excluded)"))
        }
      } else {
        checks$missing <- list(status = "OK", text = "No missing values")
      }

      # Build UI
      check_items <- lapply(names(checks), function(nm) {
        check <- checks[[nm]]
        icon_name <- switch(check$status,
                            "OK" = "check-circle",
                            "Warning" = "exclamation-triangle",
                            "Issue" = "x-circle"
        )
        icon_color <- switch(check$status,
                             "OK" = "text-success",
                             "Warning" = "text-warning",
                             "Issue" = "text-danger"
        )
        div(
          class = "d-flex align-items-center mb-2",
          bsicons::bs_icon(icon_name, class = paste("me-2", icon_color)),
          span(check$text)
        )
      })

      div(
        h6(paste0("Outcome: ", rv$outcome_var)),
        tagList(check_items)
      )
    })

    # Sample size check
    output$dq_sample_check <- renderUI({
      req(rv$data, rv$outcome_var, rv$predictor_vars)

      n_obs <- nrow(rv$data)
      n_predictors <- length(rv$predictor_vars)
      outcome <- rv$data[[rv$outcome_var]]

      checks <- list()

      # Overall sample size
      if (n_obs < 100) {
        checks$overall <- list(status = "Warning", text = paste0("Small sample (n=", n_obs, ") - tree may be unstable"))
      } else if (n_obs < 200) {
        checks$overall <- list(status = "OK", text = paste0("Adequate sample (n=", n_obs, ")"))
      } else {
        checks$overall <- list(status = "OK", text = paste0("Good sample size (n=", n_obs, ")"))
      }

      # Per outcome level (for classification)
      if (is.factor(outcome) || is.character(outcome)) {
        min_level_n <- min(table(outcome))
        if (min_level_n < 30) {
          checks$per_level <- list(status = "Warning", text = paste0("Smallest outcome group has only ", min_level_n, " cases (recommend 30+)"))
        } else if (min_level_n < 50) {
          checks$per_level <- list(status = "OK", text = paste0("Smallest outcome group: ", min_level_n, " cases"))
        } else {
          checks$per_level <- list(status = "OK", text = paste0("Good outcome group sizes (min: ", min_level_n, ")"))
        }
      }

      # Observations per predictor
      obs_per_pred <- round(n_obs / max(n_predictors, 1))
      if (obs_per_pred < 10) {
        checks$ratio <- list(status = "Warning", text = paste0("Only ", obs_per_pred, " observations per predictor (recommend 10+)"))
      } else {
        checks$ratio <- list(status = "OK", text = paste0(obs_per_pred, " observations per predictor"))
      }

      # Build UI
      check_items <- lapply(names(checks), function(nm) {
        check <- checks[[nm]]
        icon_name <- switch(check$status,
                            "OK" = "check-circle",
                            "Warning" = "exclamation-triangle",
                            "Issue" = "x-circle"
        )
        icon_color <- switch(check$status,
                             "OK" = "text-success",
                             "Warning" = "text-warning",
                             "Issue" = "text-danger"
        )
        div(
          class = "d-flex align-items-center mb-2",
          bsicons::bs_icon(icon_name, class = paste("me-2", icon_color)),
          span(check$text)
        )
      })

      div(tagList(check_items))
    })

    # Recommendations
    output$dq_recommendations <- renderUI({
      req(data_quality())
      dq <- data_quality()

      recommendations <- list()

      # Check for constant variables
      constant_vars <- dq$variance$Variable[dq$variance$Variance_Status == "Constant"]
      if (length(constant_vars) > 0) {
        recommendations <- c(recommendations, list(
          div(
            class = "d-flex mb-2",
            bsicons::bs_icon("x-circle", class = "text-danger me-2 flex-shrink-0"),
            span(
              tags$strong("Remove constant variables: "),
              paste(constant_vars, collapse = ", "),
              " - these have only one value and cannot contribute to the model."
            )
          )
        ))
      }

      # Check for high missing
      high_missing <- dq$missing$Variable[dq$missing$Pct_Missing > 50]
      if (length(high_missing) > 0) {
        recommendations <- c(recommendations, list(
          div(
            class = "d-flex mb-2",
            bsicons::bs_icon("exclamation-triangle", class = "text-warning me-2 flex-shrink-0"),
            span(
              tags$strong("Review high-missingness variables: "),
              paste(high_missing, collapse = ", "),
              " - >50% missing. This may be due to skip logic (OK) or data issues (investigate)."
            )
          )
        ))
      }

      # Check for many correlated pairs
      if (nrow(dq$correlation) > 5) {
        recommendations <- c(recommendations, list(
          div(
            class = "d-flex mb-2",
            bsicons::bs_icon("info-circle", class = "text-info me-2 flex-shrink-0"),
            span(
              tags$strong("Multiple correlated predictors detected. "),
              "This is common in attitudinal surveys. The tree will select the most predictive variable at each split, but be aware that similar variables may be interchangeable."
            )
          )
        ))
      }

      # Check for extreme outliers (>5% extreme)
      if (nrow(dq$outliers) > 0) {
        extreme_vars <- dq$outliers$Variable[dq$outliers$Pct_Extreme > 5]
        if (length(extreme_vars) > 0) {
          recommendations <- c(recommendations, list(
            div(
              class = "d-flex mb-2",
              bsicons::bs_icon("exclamation-triangle", class = "text-warning me-2 flex-shrink-0"),
              span(
                tags$strong("Extreme outliers detected: "),
                paste(extreme_vars, collapse = ", "),
                " \u2014 review for data entry errors or consider impact on tree splits."
              )
            )
          ))
        }

        mild_vars <- dq$outliers$Variable[dq$outliers$Pct_Mild > 10 & dq$outliers$Pct_Extreme <= 5]
        if (length(mild_vars) > 0) {
          recommendations <- c(recommendations, list(
            div(
              class = "d-flex mb-2",
              bsicons::bs_icon("info-circle", class = "text-info me-2 flex-shrink-0"),
              span(
                tags$strong("Mild outliers in: "),
                paste(mild_vars, collapse = ", "),
                " \u2014 decision trees are generally robust to outliers, but skewed distributions may affect split points."
              )
            )
          ))
        }
      }

      # Sample size
      if (dq$n_obs < 100) {
        recommendations <- c(recommendations, list(
          div(
            class = "d-flex mb-2",
            bsicons::bs_icon("exclamation-triangle", class = "text-warning me-2 flex-shrink-0"),
            span(
              tags$strong("Small sample size (n=", dq$n_obs, "). "),
              "Consider using higher cp values (0.02-0.05) to prevent overfitting and ensure stable results."
            )
          )
        ))
      }

      if (length(recommendations) == 0) {
        recommendations <- list(
          div(
            class = "d-flex mb-2",
            bsicons::bs_icon("check-circle", class = "text-success me-2 flex-shrink-0"),
            span(tags$strong("Data looks good! "), "No major issues detected. You're ready to build your decision tree.")
          )
        )
      }

      tagList(recommendations)
    })
  })
}
