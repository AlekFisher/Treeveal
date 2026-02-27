# mod_export.R
# Export handlers: PPTX, HTML, PDF, and image downloads
# No own UI — buttons live in mod_tree_viz.R dropdown

# =============================================================================
# Shared Helpers
# =============================================================================

#' Render the decision tree plot to a temp PNG file
render_tree_to_file <- function(model, outcome_var,
                                width = 10, height = 7, res = 150) {
  path <- tempfile(fileext = ".png")
  png(path, width = width, height = height, units = "in", res = res)
  render_tree_plot(model, title = "")
  dev.off()
  path
}

#' Render variable importance chart to a temp PNG file (or NULL)
render_importance_to_file <- function(model, data_dict = NULL,
                                      width = 10, height = 6, res = 150) {
  if (is.null(model$variable.importance)) return(NULL)

  importance_df <- data.frame(
    Variable = names(model$variable.importance),
    Importance = round(model$variable.importance, 2)
  ) |>
    dplyr::arrange(dplyr::desc(Importance)) |>
    head(10)

  if (!is.null(data_dict)) {
    importance_df$Label <- sapply(importance_df$Variable, function(v) {
      match_idx <- match(v, data_dict$variable)
      if (!is.na(match_idx)) data_dict$label[match_idx] else v
    })
    importance_df$DisplayName <- importance_df$Label
  } else {
    importance_df$DisplayName <- importance_df$Variable
  }

  importance_df <- importance_df |>
    dplyr::mutate(DisplayName = factor(DisplayName, levels = rev(DisplayName)))

  path <- tempfile(fileext = ".png")
  png(path, width = width, height = height, units = "in", res = res)
  p <- ggplot(importance_df, aes(x = Importance, y = DisplayName)) +
    geom_col(fill = THEME_CHART_PRIMARY) +
    theme_minimal(base_size = 14) +
    theme(
      axis.title.y = element_blank(),
      panel.grid.major.y = element_blank()
    ) +
    labs(x = "Importance Score")
  print(p)
  dev.off()
  path
}

#' Get formatted model statistics text
get_model_stats_text <- function(model, data, outcome_var,
                                 cp, minbucket, maxdepth) {
  n_nodes <- sum(model$frame$var == "<leaf>")

  if (model$method == "class") {
    pred <- predict(model, type = "class")
    actual <- data[[outcome_var]]
    cm <- table(Predicted = pred, Actual = actual)
    accuracy <- sum(diag(cm)) / sum(cm)

    paste0(
      "Accuracy: ", round(accuracy * 100, 1), "%\n",
      "Terminal Nodes: ", n_nodes, "\n",
      "Training Observations: ", nrow(data), "\n",
      "Complexity Parameter: ", cp, "\n",
      "Minimum Bucket Size: ", minbucket, "\n",
      "Maximum Depth: ", maxdepth
    )
  } else {
    paste0(
      "Terminal Nodes: ", n_nodes, "\n",
      "Training Observations: ", nrow(data), "\n",
      "Complexity Parameter: ", cp, "\n",
      "Minimum Bucket Size: ", minbucket, "\n",
      "Maximum Depth: ", maxdepth
    )
  }
}

#' Get decision rules as a text string
get_decision_rules_text <- function(model, max_lines = 30) {
  rules <- capture.output(
    rpart.plot::rpart.rules(model, style = "tall", cover = TRUE, roundint = FALSE)
  )
  rules_text <- paste(head(rules, max_lines), collapse = "\n")
  if (length(rules) > max_lines) {
    rules_text <- paste0(rules_text, "\n\n... (", length(rules) - max_lines, " more lines)")
  }
  rules_text
}

#' Convert a file to a base64 data URI
file_to_data_uri <- function(file_path) {
  raw_data <- readBin(file_path, "raw", file.info(file_path)$size)
  encoded <- base64enc::base64encode(raw_data)
  ext <- tolower(tools::file_ext(file_path))
  mime <- switch(ext,
    png = "image/png",
    jpg = , jpeg = "image/jpeg",
    svg = "image/svg+xml",
    "application/octet-stream"
  )
  paste0("data:", mime, ";base64,", encoded)
}

# =============================================================================
# PowerPoint Export
# =============================================================================

create_pptx_handler <- function(rv) {
  downloadHandler(
    filename = function() {
      paste0("decision_tree_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".pptx")
    },
    content = function(file) {
      req(rv$model, rv$data, rv$outcome_var)

      withProgress(message = "Generating PowerPoint...", value = 0, {

        pptx <- officer::read_pptx()

        # --- Slide 1: Title ---
        incProgress(0.2, detail = "Creating title slide...")
        pptx <- officer::add_slide(pptx, layout = "Title Slide", master = "Office Theme")
        pptx <- officer::ph_with(pptx, value = "Decision Tree Analysis",
                                 location = officer::ph_location_type(type = "ctrTitle"))
        pptx <- officer::ph_with(pptx,
                                 value = paste0("Outcome: ", rv$outcome_var, "\n",
                                                format(Sys.Date(), "%B %d, %Y")),
                                 location = officer::ph_location_type(type = "subTitle"))

        # --- Slide 2: Tree Visualization ---
        incProgress(0.2, detail = "Adding tree visualization...")
        pptx <- officer::add_slide(pptx, layout = "Title and Content", master = "Office Theme")
        pptx <- officer::ph_with(pptx, value = "Decision Tree",
                                 location = officer::ph_location_type(type = "title"))

        tree_temp <- render_tree_to_file(rv$model, rv$outcome_var)
        pptx <- officer::ph_with(pptx,
                                 value = officer::external_img(tree_temp, width = 9, height = 6),
                                 location = officer::ph_location_type(type = "body"))

        # --- Slide 3: Variable Importance ---
        incProgress(0.2, detail = "Adding variable importance...")
        imp_temp <- render_importance_to_file(rv$model, rv$data_dict)
        if (!is.null(imp_temp)) {
          pptx <- officer::add_slide(pptx, layout = "Title and Content", master = "Office Theme")
          pptx <- officer::ph_with(pptx, value = "Variable Importance",
                                   location = officer::ph_location_type(type = "title"))
          pptx <- officer::ph_with(pptx,
                                   value = officer::external_img(imp_temp, width = 9, height = 5.5),
                                   location = officer::ph_location_type(type = "body"))
        }

        # --- Slide 4: Model Performance (classification) ---
        incProgress(0.2, detail = "Adding model statistics...")
        if (rv$model$method == "class") {
          pptx <- officer::add_slide(pptx, layout = "Title and Content", master = "Office Theme")
          pptx <- officer::ph_with(pptx, value = "Model Performance",
                                   location = officer::ph_location_type(type = "title"))

          stats_text <- get_model_stats_text(
            rv$model, rv$data, rv$outcome_var, rv$cp, rv$minbucket, rv$maxdepth
          )
          performance_text <- paste0("Model Statistics\n\n",
                                     gsub("\n", "\n\u2022 ", paste0("\u2022 ", stats_text)))
          pptx <- officer::ph_with(pptx, value = performance_text,
                                   location = officer::ph_location_type(type = "body"))
        }

        # --- Slide 5: Decision Rules ---
        incProgress(0.1, detail = "Adding decision rules...")
        pptx <- officer::add_slide(pptx, layout = "Title and Content", master = "Office Theme")
        pptx <- officer::ph_with(pptx, value = "Decision Rules",
                                 location = officer::ph_location_type(type = "title"))

        rules_text <- get_decision_rules_text(rv$model)
        pptx <- officer::ph_with(pptx, value = rules_text,
                                 location = officer::ph_location_type(type = "body"))

        # --- Slide 6: Disclaimer ---
        incProgress(0.1, detail = "Finalizing...")
        pptx <- officer::add_slide(pptx, layout = "Title and Content", master = "Office Theme")
        pptx <- officer::ph_with(pptx, value = "Notes & Disclaimer",
                                 location = officer::ph_location_type(type = "title"))

        disclaimer_text <- paste0(
          "Analysis Details\n\n",
          "\u2022 Generated: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n",
          "\u2022 Tool: Decision Tree Analysis\n\n",
          "Disclaimer\n\n",
          "This analysis is provided for informational purposes. ",
          "Results should be validated by qualified analysts and interpreted ",
          "in the context of the specific research objectives and data limitations. ",
          "Decision trees are sensitive to the data used and parameter settings."
        )
        pptx <- officer::ph_with(pptx, value = disclaimer_text,
                                 location = officer::ph_location_type(type = "body"))

        print(pptx, target = file)

        # Clean up
        unlink(tree_temp)
        if (!is.null(imp_temp)) unlink(imp_temp)
      })
    }
  )
}

# =============================================================================
# HTML Report
# =============================================================================

#' CSS for self-contained HTML reports
report_css <- function() {
  paste0(
    "body { font-family: 'Inter', -apple-system, BlinkMacSystemFont, 'Segoe UI', system-ui, sans-serif; ",
    "color: ", THEME_TEXT_PRIMARY, "; background: ", THEME_BG_PRIMARY, "; ",
    "max-width: 900px; margin: 0 auto; padding: 2rem; line-height: 1.6; -webkit-font-smoothing: antialiased; }",
    "h1 { font-size: 1.5rem; font-weight: 600; letter-spacing: -0.02em; margin-bottom: 0.25rem; }",
    "h2 { font-size: 1.125rem; font-weight: 600; letter-spacing: -0.02em; color: ", THEME_TEXT_PRIMARY, "; ",
    "margin-top: 2.5rem; padding-bottom: 0.5rem; border-bottom: 1px solid ", THEME_BORDER_LIGHT, "; }",
    ".subtitle { color: ", THEME_TEXT_SECONDARY, "; font-size: 0.875rem; margin-bottom: 2rem; }",
    ".section { margin-bottom: 2rem; }",
    "img { max-width: 100%; height: auto; border-radius: 8px; border: 1px solid ", THEME_BORDER_LIGHT, "; }",
    "table { width: 100%; border-collapse: collapse; font-size: 0.875rem; margin: 1rem 0; }",
    "th { text-align: left; font-weight: 500; font-size: 0.75rem; text-transform: uppercase; ",
    "letter-spacing: 0.04em; color: ", THEME_TEXT_SECONDARY, "; padding: 0.625rem 0.75rem; ",
    "border-bottom: 1px solid ", THEME_BORDER, "; }",
    "td { padding: 0.625rem 0.75rem; border-bottom: 1px solid ", THEME_BORDER_LIGHT, "; }",
    "pre { background: ", THEME_BG_SECONDARY, "; border: 1px solid ", THEME_BORDER_LIGHT, "; ",
    "border-radius: 6px; padding: 1rem; font-size: 0.8125rem; overflow-x: auto; ",
    "font-family: 'JetBrains Mono', 'SF Mono', 'Fira Code', 'Consolas', monospace; }",
    ".chat-user { background: ", THEME_ACCENT, "; color: white; padding: 0.75rem 1rem; ",
    "border-radius: 10px 10px 4px 10px; margin: 0.5rem 0 0.5rem 20%; }",
    ".chat-ai { background: ", THEME_BG_PRIMARY, "; border: 1px solid ", THEME_BORDER_LIGHT, "; ",
    "border-left: 3px solid #93c5fd; padding: 0.75rem 1rem; ",
    "border-radius: 10px 10px 10px 4px; margin: 0.5rem 20% 0.5rem 0; }",
    ".footer { margin-top: 3rem; padding-top: 1.5rem; border-top: 1px solid ", THEME_BORDER_LIGHT, "; ",
    "color: ", THEME_TEXT_SECONDARY, "; font-size: 0.75rem; }",
    ".stat-grid { display: grid; grid-template-columns: repeat(auto-fill, minmax(200px, 1fr)); gap: 0.75rem; }",
    ".stat-item { background: ", THEME_BG_SECONDARY, "; border-radius: 6px; padding: 0.75rem 1rem; }",
    ".stat-label { font-size: 0.6875rem; font-weight: 500; text-transform: uppercase; ",
    "letter-spacing: 0.04em; color: ", THEME_TEXT_SECONDARY, "; }",
    ".stat-value { font-size: 1.25rem; font-weight: 600; }"
  )
}

#' Convert chat history list to HTML tags
format_chat_history_html <- function(chat_history) {
  if (is.null(chat_history) || length(chat_history) == 0) return(NULL)

  items <- lapply(chat_history, function(msg) {
    content_html <- markdown::markdownToHTML(
      text = msg$content, fragment.only = TRUE
    )
    if (msg$role == "user") {
      htmltools::tags$div(class = "chat-user", htmltools::HTML(content_html))
    } else {
      htmltools::tags$div(class = "chat-ai", htmltools::HTML(content_html))
    }
  })

  htmltools::tagList(items)
}

#' Build classification performance as an HTML table
build_performance_html <- function(model, data, outcome_var) {
  if (model$method != "class") return(NULL)

  pred <- predict(model, type = "class")
  actual <- data[[outcome_var]]
  cm <- table(Predicted = pred, Actual = actual)
  accuracy <- sum(diag(cm)) / sum(cm)

  # Confusion matrix table
  cm_df <- as.data.frame.matrix(cm)
  header_cells <- c(
    htmltools::tags$th(""),
    lapply(colnames(cm_df), function(cn) htmltools::tags$th(cn))
  )

  rows <- lapply(rownames(cm_df), function(rn) {
    htmltools::tags$tr(
      htmltools::tags$td(htmltools::tags$strong(rn)),
      lapply(cm_df[rn, ], function(val) htmltools::tags$td(val))
    )
  })

  htmltools::tagList(
    htmltools::tags$p(
      htmltools::tags$strong("Accuracy: "),
      paste0(round(accuracy * 100, 1), "%")
    ),
    htmltools::tags$table(
      htmltools::tags$thead(htmltools::tags$tr(header_cells)),
      htmltools::tags$tbody(rows)
    )
  )
}

#' Generate a self-contained HTML report
generate_html_report <- function(file, model, data, outcome_var, data_dict,
                                  cp, minbucket, maxdepth, chat_history) {
  # Render images to temp files, then embed as data URIs
  tree_path <- render_tree_to_file(model, outcome_var, width = 12, height = 8, res = 200)
  tree_uri <- file_to_data_uri(tree_path)
  unlink(tree_path)

  imp_path <- render_importance_to_file(model, data_dict, width = 10, height = 6, res = 200)
  imp_uri <- NULL
  if (!is.null(imp_path)) {
    imp_uri <- file_to_data_uri(imp_path)
    unlink(imp_path)
  }

  stats_text <- get_model_stats_text(model, data, outcome_var, cp, minbucket, maxdepth)
  rules_text <- get_decision_rules_text(model, max_lines = 50)
  n_nodes <- sum(model$frame$var == "<leaf>")

  # Build stat grid items
  stat_items <- list()
  if (model$method == "class") {
    pred <- predict(model, type = "class")
    actual <- data[[outcome_var]]
    cm <- table(Predicted = pred, Actual = actual)
    accuracy <- sum(diag(cm)) / sum(cm)
    stat_items <- c(stat_items, list(
      htmltools::tags$div(class = "stat-item",
        htmltools::tags$div(class = "stat-label", "Accuracy"),
        htmltools::tags$div(class = "stat-value", paste0(round(accuracy * 100, 1), "%"))
      )
    ))
  }
  stat_items <- c(stat_items, list(
    htmltools::tags$div(class = "stat-item",
      htmltools::tags$div(class = "stat-label", "Terminal Nodes"),
      htmltools::tags$div(class = "stat-value", n_nodes)
    ),
    htmltools::tags$div(class = "stat-item",
      htmltools::tags$div(class = "stat-label", "Observations"),
      htmltools::tags$div(class = "stat-value", format(nrow(data), big.mark = ","))
    ),
    htmltools::tags$div(class = "stat-item",
      htmltools::tags$div(class = "stat-label", "Complexity (cp)"),
      htmltools::tags$div(class = "stat-value", cp)
    ),
    htmltools::tags$div(class = "stat-item",
      htmltools::tags$div(class = "stat-label", "Min Bucket"),
      htmltools::tags$div(class = "stat-value", minbucket)
    ),
    htmltools::tags$div(class = "stat-item",
      htmltools::tags$div(class = "stat-label", "Max Depth"),
      htmltools::tags$div(class = "stat-value", maxdepth)
    )
  ))

  # Assemble sections
  sections <- htmltools::tagList(
    # Header
    htmltools::tags$h1("Decision Tree Analysis"),
    htmltools::tags$div(class = "subtitle",
      paste0("Outcome: ", outcome_var, " \u2022 ", format(Sys.Date(), "%B %d, %Y"))
    ),

    # Tree visualization
    htmltools::tags$div(class = "section",
      htmltools::tags$h2("Decision Tree"),
      htmltools::tags$img(src = tree_uri, alt = "Decision Tree")
    ),

    # Model stats
    htmltools::tags$div(class = "section",
      htmltools::tags$h2("Model Performance"),
      htmltools::tags$div(class = "stat-grid", stat_items)
    )
  )

  # Confusion matrix for classification
  if (model$method == "class") {
    perf_html <- build_performance_html(model, data, outcome_var)
    sections <- htmltools::tagList(
      sections,
      htmltools::tags$div(class = "section",
        htmltools::tags$h2("Confusion Matrix"),
        perf_html
      )
    )
  }

  # Variable importance
  if (!is.null(imp_uri)) {
    sections <- htmltools::tagList(
      sections,
      htmltools::tags$div(class = "section",
        htmltools::tags$h2("Variable Importance"),
        htmltools::tags$img(src = imp_uri, alt = "Variable Importance")
      )
    )
  }

  # Decision rules
  sections <- htmltools::tagList(
    sections,
    htmltools::tags$div(class = "section",
      htmltools::tags$h2("Decision Rules"),
      htmltools::tags$pre(rules_text)
    )
  )

  # AI interpretation
  chat_html <- format_chat_history_html(chat_history)
  if (!is.null(chat_html)) {
    sections <- htmltools::tagList(
      sections,
      htmltools::tags$div(class = "section",
        htmltools::tags$h2("AI Interpretation"),
        chat_html
      )
    )
  }

  # Footer
  sections <- htmltools::tagList(
    sections,
    htmltools::tags$div(class = "footer",
      htmltools::tags$p(
        paste0("Generated: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
               " \u2022 Dendro Decision Tree Analysis")
      ),
      htmltools::tags$p(
        "This analysis is provided for informational purposes. ",
        "Results should be validated by qualified analysts and interpreted ",
        "in the context of the specific research objectives and data limitations."
      )
    )
  )

  # Full HTML document
  page <- htmltools::tags$html(
    htmltools::tags$head(
      htmltools::tags$meta(charset = "utf-8"),
      htmltools::tags$meta(name = "viewport", content = "width=device-width, initial-scale=1"),
      htmltools::tags$title(paste("Decision Tree:", outcome_var)),
      htmltools::tags$style(htmltools::HTML(report_css()))
    ),
    htmltools::tags$body(sections)
  )

  htmltools::save_html(page, file = file)
}

create_html_handler <- function(rv) {
  downloadHandler(
    filename = function() {
      paste0("decision_tree_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".html")
    },
    content = function(file) {
      req(rv$model, rv$data, rv$outcome_var)
      withProgress(message = "Generating HTML report...", value = 0.3, {
        generate_html_report(
          file = file,
          model = rv$model,
          data = rv$data,
          outcome_var = rv$outcome_var,
          data_dict = rv$data_dict,
          cp = rv$cp,
          minbucket = rv$minbucket,
          maxdepth = rv$maxdepth,
          chat_history = rv$chat_history
        )
        incProgress(0.7, detail = "Done")
      })
    }
  )
}

# =============================================================================
# Image Exports
# =============================================================================

create_tree_image_handler <- function(rv) {
  downloadHandler(
    filename = function() {
      paste0("tree_plot_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")
    },
    content = function(file) {
      req(rv$model)
      path <- render_tree_to_file(rv$model, rv$outcome_var,
                                  width = 12, height = 8, res = 200)
      file.copy(path, file)
      unlink(path)
    }
  )
}

create_importance_image_handler <- function(rv) {
  downloadHandler(
    filename = function() {
      paste0("importance_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")
    },
    content = function(file) {
      req(rv$model)
      path <- render_importance_to_file(rv$model, rv$data_dict,
                                        width = 10, height = 6, res = 200)
      if (is.null(path)) {
        showNotification("No variable importance data available for this model.",
                         type = "warning", duration = 5)
        # Write a minimal placeholder so download doesn't fail
        png(file, width = 400, height = 200)
        plot.new()
        text(0.5, 0.5, "No variable importance data available", cex = 1.2)
        dev.off()
        return()
      }
      file.copy(path, file)
      unlink(path)
    }
  )
}
