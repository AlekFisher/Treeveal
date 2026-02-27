# mod_model_details.R
# Model Details tab: stats, importance, confusion matrix, RF importance, comparison

model_details_ui <- function(id) {
  ns <- NS(id)

  layout_columns(
    col_widths = c(6, 6),

    # Left column
    layout_columns(
      col_widths = c(12),

      card(
        card_header("Model Statistics"),
        card_body(
          conditionalPanel(
            condition = "output.model_built",
            verbatimTextOutput(ns("model_stats"))
          ),
          conditionalPanel(
            condition = "!output.model_built",
            div(class = "text-center py-3 text-muted", "Build a model to see statistics")
          )
        )
      ),

      card(
        card_header("Variable Importance"),
        card_body(
          conditionalPanel(
            condition = "output.model_built",
            plotOutput(ns("var_importance"), height = "200px")
          ),
          conditionalPanel(
            condition = "!output.model_built",
            div(class = "text-center py-3 text-muted", "Build a model to see importance")
          )
        )
      ),

      card(
        card_header("Confusion Matrix"),
        card_body(
          conditionalPanel(
            condition = "output.model_built",
            DTOutput(ns("confusion_matrix"))
          ),
          conditionalPanel(
            condition = "!output.model_built",
            div(class = "text-center py-3 text-muted", "Build a classification model to see matrix")
          )
        )
      )
    ),

    # Right column
    layout_columns(
      col_widths = c(12),

      card(
        card_header(
          class = "d-flex align-items-center",
          "Random Forest Importance",
          tags$span(
            class = "ms-2 badge bg-secondary",
            style = "font-weight: normal; font-size: 0.7em;",
            "Validation"
          )
        ),
        card_body(
          conditionalPanel(
            condition = "output.model_built",
            p(class = "text-muted small", "Random Forest provides a complementary importance measure based on 500 trees. Variables important in both methods are more reliably predictive."),
            plotOutput(ns("rf_importance"), height = "280px")
          ),
          conditionalPanel(
            condition = "!output.model_built",
            div(class = "text-center py-3 text-muted", "Build a model to see Random Forest importance")
          )
        )
      ),

      card(
        card_header("Importance Comparison"),
        card_body(
          conditionalPanel(
            condition = "output.model_built",
            p(class = "text-muted small", "Side-by-side comparison of variable rankings from both methods."),
            DTOutput(ns("importance_comparison"))
          ),
          conditionalPanel(
            condition = "!output.model_built",
            div(class = "text-center py-3 text-muted", "Build a model to compare importance")
          )
        )
      )
    )
  )
}

model_details_server <- function(id, rv) {
  moduleServer(id, function(input, output, session) {

    output$var_importance <- renderPlot({
      req(rv$model)

      if (is.null(rv$model$variable.importance)) {
        return(NULL)
      }

      importance_df <- data.frame(
        Variable = names(rv$model$variable.importance),
        Importance = rv$model$variable.importance
      ) |>
        dplyr::arrange(dplyr::desc(Importance)) |>
        head(10) |>
        dplyr::mutate(Variable = factor(Variable, levels = rev(Variable)))

      ggplot(importance_df, aes(x = Importance, y = Variable)) +
        geom_col(fill = "#6366f1") +
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
      req(rv$model, rv$data, rv$outcome_var)

      if (rv$model$method != "class") {
        return(NULL)
      }

      pred <- predict(rv$model, type = "class")
      actual <- rv$data[[rv$outcome_var]]

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

    # Random Forest importance
    rf_model <- reactive({
      req(rv$model, rv$data, rv$outcome_var, rv$predictor_vars)

      predictors <- setdiff(rv$predictor_vars, rv$outcome_var)
      model_data <- rv$data[, c(rv$outcome_var, predictors), drop = FALSE]
      model_data <- model_data[complete.cases(model_data), ]

      if (nrow(model_data) < 50) return(NULL)

      formula_str <- paste(rv$outcome_var, "~", paste(predictors, collapse = " + "))
      model_formula <- as.formula(formula_str)

      tryCatch({
        withProgress(message = "Computing Random Forest importance...", value = 0.5, {
          rf <- randomForest::randomForest(
            formula = model_formula,
            data = model_data,
            ntree = 500,
            importance = TRUE,
            na.action = na.omit
          )
          rf
        })
      }, error = function(e) {
        NULL
      })
    })

    output$rf_importance <- renderPlot({
      rf <- rf_model()

      if (is.null(rf)) {
        ggplot() +
          annotate("text", x = 0.5, y = 0.5,
                   label = "Random Forest could not be computed.\n\nPossible reasons:\n\u2022 Fewer than 50 complete cases\n\u2022 Data type incompatibility\n\u2022 Too many factor levels",
                   size = 4, color = "#86868b", hjust = 0.5, vjust = 0.5) +
          theme_void() +
          xlim(0, 1) + ylim(0, 1)
      } else {
        if (rf$type == "classification") {
          imp <- randomForest::importance(rf, type = 2)
        } else {
          imp <- randomForest::importance(rf, type = 1)
        }
        imp_df <- data.frame(
          Variable = rownames(imp),
          Importance = imp[, 1]
        )

        imp_df <- imp_df |>
          dplyr::arrange(dplyr::desc(Importance)) |>
          head(10) |>
          dplyr::mutate(Variable = factor(Variable, levels = rev(Variable)))

        # Add labels from dictionary if available
        if (!is.null(rv$data_dict)) {
          imp_df$Label <- sapply(as.character(imp_df$Variable), function(v) {
            match_idx <- match(v, rv$data_dict$variable)
            if (!is.na(match_idx)) rv$data_dict$label[match_idx] else v
          })
          imp_df$DisplayName <- ifelse(nchar(imp_df$Label) > 0, imp_df$Label, as.character(imp_df$Variable))
          imp_df$DisplayName <- factor(imp_df$DisplayName, levels = rev(imp_df$DisplayName))
        } else {
          imp_df$DisplayName <- imp_df$Variable
        }

        ggplot(imp_df, aes(x = Importance, y = DisplayName)) +
          geom_col(fill = "#8b5cf6") +
          theme_minimal(base_size = 12) +
          theme(
            axis.title.y = element_blank(),
            panel.grid.major.y = element_blank()
          ) +
          labs(x = "Importance (Mean Decrease Gini)")
      }
    }, res = 96)

    output$importance_comparison <- renderDT({
      req(rv$model)

      rf <- rf_model()

      # If RF failed, show only decision tree importance
      if (is.null(rf)) {
        if (!is.null(rv$model$variable.importance)) {
          dt_imp <- data.frame(
            Variable = names(rv$model$variable.importance),
            DT_Importance = round(rv$model$variable.importance, 2),
            stringsAsFactors = FALSE
          )
          dt_imp$DT_Rank <- rank(-dt_imp$DT_Importance, ties.method = "min")
          dt_imp <- dt_imp[order(dt_imp$DT_Rank), ]

          # Add labels from dictionary
          if (!is.null(rv$data_dict)) {
            dt_imp$Label <- sapply(dt_imp$Variable, function(v) {
              match_idx <- match(v, rv$data_dict$variable)
              if (!is.na(match_idx)) rv$data_dict$label[match_idx] else ""
            })
            dt_imp <- dt_imp[, c("Variable", "Label", "DT_Rank", "DT_Importance")]
            names(dt_imp) <- c("Variable", "Label", "Tree Rank", "Tree Importance")
          } else {
            dt_imp <- dt_imp[, c("Variable", "DT_Rank", "DT_Importance")]
            names(dt_imp) <- c("Variable", "Tree Rank", "Tree Importance")
          }

          dt_imp <- head(dt_imp, 15)

          return(datatable(
            dt_imp,
            options = list(
              dom = 't',
              paging = FALSE,
              scrollX = TRUE,
              columnDefs = list(
                list(className = 'dt-center', targets = '_all'),
                list(visible = FALSE, targets = if ("Label" %in% names(dt_imp)) 3 else 2)
              )
            ),
            class = 'compact hover',
            rownames = FALSE,
            caption = "Note: Random Forest comparison unavailable"
          ) %>%
            formatStyle(
              'Tree Rank',
              backgroundColor = styleInterval(
                c(3, 6, 10),
                c('#d1fae5', '#fef3c7', '#fde68a', '#f1f0fb')
              ),
              fontWeight = styleInterval(c(3), c('bold', 'normal'))
            ) %>%
            formatStyle(
              'Variable',
              fontWeight = 'bold',
              fontSize = '0.9em'
            )
          )
        } else {
          return(NULL)
        }
      }

      # Decision Tree importance
      if (!is.null(rv$model$variable.importance)) {
        dt_imp <- data.frame(
          Variable = names(rv$model$variable.importance),
          DT_Importance = round(rv$model$variable.importance, 2),
          stringsAsFactors = FALSE
        )
        dt_imp$DT_Rank <- rank(-dt_imp$DT_Importance, ties.method = "min")
      } else {
        return(NULL)
      }

      # Random Forest importance
      if (rf$type == "classification") {
        rf_imp_raw <- randomForest::importance(rf, type = 2)
      } else {
        rf_imp_raw <- randomForest::importance(rf, type = 1)
      }
      rf_imp <- data.frame(
        Variable = rownames(rf_imp_raw),
        RF_Importance = round(rf_imp_raw[, 1], 2),
        stringsAsFactors = FALSE
      )
      rf_imp$RF_Rank <- rank(-rf_imp$RF_Importance, ties.method = "min")

      # Merge
      comparison <- merge(dt_imp, rf_imp, by = "Variable", all = TRUE)
      comparison[is.na(comparison)] <- 0

      # Add labels from dictionary
      if (!is.null(rv$data_dict)) {
        comparison$Label <- sapply(comparison$Variable, function(v) {
          match_idx <- match(v, rv$data_dict$variable)
          if (!is.na(match_idx)) rv$data_dict$label[match_idx] else ""
        })
        comparison <- comparison[, c("Variable", "Label", "DT_Rank", "RF_Rank", "DT_Importance", "RF_Importance")]
      } else {
        comparison <- comparison[, c("Variable", "DT_Rank", "RF_Rank", "DT_Importance", "RF_Importance")]
      }

      # Sort by average rank
      comparison$Avg_Rank <- (comparison$DT_Rank + comparison$RF_Rank) / 2
      comparison <- comparison[order(comparison$Avg_Rank), ]
      comparison$Avg_Rank <- NULL

      # Rename columns for display
      if ("Label" %in% names(comparison)) {
        names(comparison) <- c("Variable", "Label", "Tree Rank", "RF Rank", "Tree Imp", "RF Imp")
      } else {
        names(comparison) <- c("Variable", "Tree Rank", "RF Rank", "Tree Imp", "RF Imp")
      }

      comparison <- head(comparison, 15)

      datatable(
        comparison,
        options = list(
          dom = 't',
          paging = FALSE,
          scrollX = TRUE,
          columnDefs = list(
            list(className = 'dt-center', targets = '_all'),
            list(visible = FALSE, targets = if ("Label" %in% names(comparison)) c(4, 5) else c(3, 4))
          )
        ),
        class = 'compact hover',
        rownames = FALSE
      ) %>%
        formatStyle(
          'Tree Rank',
          backgroundColor = styleInterval(
            c(3, 6, 10),
            c('#d1fae5', '#fef3c7', '#fde68a', '#f1f0fb')
          ),
          fontWeight = styleInterval(c(3), c('bold', 'normal'))
        ) %>%
        formatStyle(
          'RF Rank',
          backgroundColor = styleInterval(
            c(3, 6, 10),
            c('#d1fae5', '#fef3c7', '#fde68a', '#f1f0fb')
          ),
          fontWeight = styleInterval(c(3), c('bold', 'normal'))
        ) %>%
        formatStyle(
          'Variable',
          fontWeight = 'bold',
          fontSize = '0.9em'
        )
    })
  })
}
