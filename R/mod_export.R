# mod_export.R
# PowerPoint export handler (called from mod_tree_viz.R)
# No own UI — the download button lives in the tree card header

create_export_handler <- function(rv) {
  downloadHandler(
    filename = function() {
      paste0("decision_tree_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".pptx")
    },
    content = function(file) {
      req(rv$model, rv$data, rv$outcome_var)

      withProgress(message = "Generating PowerPoint...", value = 0, {

        # Create PowerPoint
        pptx <- officer::read_pptx()

        # --- Slide 1: Title Slide ---
        incProgress(0.2, detail = "Creating title slide...")

        pptx <- officer::add_slide(pptx, layout = "Title Slide", master = "Office Theme")
        pptx <- officer::ph_with(pptx, value = "Decision Tree Analysis", location = officer::ph_location_type(type = "ctrTitle"))
        pptx <- officer::ph_with(pptx, value = paste0("Outcome: ", rv$outcome_var, "\n", format(Sys.Date(), "%B %d, %Y")),
                        location = officer::ph_location_type(type = "subTitle"))

        # --- Slide 2: Decision Tree Visualization ---
        incProgress(0.2, detail = "Adding tree visualization...")

        pptx <- officer::add_slide(pptx, layout = "Title and Content", master = "Office Theme")
        pptx <- officer::ph_with(pptx, value = "Decision Tree", location = officer::ph_location_type(type = "title"))

        tree_temp <- tempfile(fileext = ".png")
        png(tree_temp, width = 10, height = 7, units = "in", res = 150)
        extra_val <- if (rv$model$method == "class") 104 else 101
        rpart.plot::rpart.plot(
          rv$model,
          type = 4,
          extra = extra_val,
          under = TRUE,
          fallen.leaves = TRUE,
          roundint = FALSE,
          box.palette = "BuGn",
          shadow.col = "gray",
          main = ""
        )
        dev.off()

        pptx <- officer::ph_with(pptx, value = officer::external_img(tree_temp, width = 9, height = 6),
                        location = officer::ph_location_type(type = "body"))

        # --- Slide 3: Variable Importance ---
        incProgress(0.2, detail = "Adding variable importance...")

        if (!is.null(rv$model$variable.importance)) {
          pptx <- officer::add_slide(pptx, layout = "Title and Content", master = "Office Theme")
          pptx <- officer::ph_with(pptx, value = "Variable Importance", location = officer::ph_location_type(type = "title"))

          importance_df <- data.frame(
            Variable = names(rv$model$variable.importance),
            Importance = round(rv$model$variable.importance, 2)
          ) |>
            dplyr::arrange(dplyr::desc(Importance)) |>
            head(10)

          # Add labels from dictionary if available
          if (!is.null(rv$data_dict)) {
            importance_df$Label <- sapply(importance_df$Variable, function(v) {
              match_idx <- match(v, rv$data_dict$variable)
              if (!is.na(match_idx)) rv$data_dict$label[match_idx] else v
            })
            importance_df$DisplayName <- importance_df$Label
          } else {
            importance_df$DisplayName <- importance_df$Variable
          }

          importance_df <- importance_df |>
            dplyr::mutate(DisplayName = factor(DisplayName, levels = rev(DisplayName)))

          imp_temp <- tempfile(fileext = ".png")
          png(imp_temp, width = 10, height = 6, units = "in", res = 150)
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

          pptx <- officer::ph_with(pptx, value = officer::external_img(imp_temp, width = 9, height = 5.5),
                          location = officer::ph_location_type(type = "body"))
        }

        # --- Slide 4: Model Performance (for classification) ---
        incProgress(0.2, detail = "Adding model statistics...")

        if (rv$model$method == "class") {
          pptx <- officer::add_slide(pptx, layout = "Title and Content", master = "Office Theme")
          pptx <- officer::ph_with(pptx, value = "Model Performance", location = officer::ph_location_type(type = "title"))

          pred <- predict(rv$model, type = "class")
          actual <- rv$data[[rv$outcome_var]]
          cm <- table(Predicted = pred, Actual = actual)
          accuracy <- sum(diag(cm)) / sum(cm)

          n_nodes <- sum(rv$model$frame$var == "<leaf>")
          performance_text <- paste0(
            "Model Statistics\n\n",
            "\u2022 Accuracy: ", round(accuracy * 100, 1), "%\n",
            "\u2022 Terminal Nodes: ", n_nodes, "\n",
            "\u2022 Training Observations: ", nrow(rv$data), "\n",
            "\u2022 Complexity Parameter: ", rv$cp, "\n",
            "\u2022 Minimum Bucket Size: ", rv$minbucket, "\n",
            "\u2022 Maximum Depth: ", rv$maxdepth
          )

          pptx <- officer::ph_with(pptx, value = performance_text, location = officer::ph_location_type(type = "body"))
        }

        # --- Slide 5: Key Decision Rules ---
        incProgress(0.1, detail = "Adding decision rules...")

        pptx <- officer::add_slide(pptx, layout = "Title and Content", master = "Office Theme")
        pptx <- officer::ph_with(pptx, value = "Decision Rules", location = officer::ph_location_type(type = "title"))

        rules <- capture.output(rpart.plot::rpart.rules(rv$model, style = "tall", cover = TRUE))
        rules_text <- paste(head(rules, 30), collapse = "\n")
        if (length(rules) > 30) {
          rules_text <- paste0(rules_text, "\n\n... (", length(rules) - 30, " more lines)")
        }

        pptx <- officer::ph_with(pptx, value = rules_text, location = officer::ph_location_type(type = "body"))

        # --- Slide 6: Disclaimer ---
        incProgress(0.1, detail = "Finalizing...")

        pptx <- officer::add_slide(pptx, layout = "Title and Content", master = "Office Theme")
        pptx <- officer::ph_with(pptx, value = "Notes & Disclaimer", location = officer::ph_location_type(type = "title"))

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

        pptx <- officer::ph_with(pptx, value = disclaimer_text, location = officer::ph_location_type(type = "body"))

        # Save the file
        print(pptx, target = file)

        # Clean up temp files
        unlink(tree_temp)
        if (exists("imp_temp")) unlink(imp_temp)
      })
    }
  )
}
