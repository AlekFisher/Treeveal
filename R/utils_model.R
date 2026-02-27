# utils_model.R
# Pure functions for model building and analysis

#' Build a decision tree model
build_decision_tree <- function(data, outcome_var, predictors, cp, minbucket, maxdepth) {
  # Convert binary numeric variables to factors for cleaner splits
  model_data <- convert_binary_to_factor(data, c(outcome_var, predictors))

  # Build formula
  formula_str <- paste(outcome_var, "~", paste(predictors, collapse = " + "))
  model_formula <- as.formula(formula_str)

  # Fit model
  model <- rpart::rpart(
    formula = model_formula,
    data = model_data,
    method = ifelse(is.factor(model_data[[outcome_var]]), "class", "anova"),
    control = rpart::rpart.control(
      cp = cp,
      minbucket = minbucket,
      maxdepth = maxdepth
    )
  )

  model
}

#' Render a static decision tree plot (shared by mod_tree_viz and mod_export)
render_tree_plot <- function(model, title = "") {
  n_classes <- length(attr(model, "ylevels"))
  is_classification <- model$method == "class"
  extra_val <- if (is_classification) 104 else 101

  if (is_classification && n_classes > 0) {
    palette <- TREE_PALETTE_CLASSIFICATION[seq_len(min(n_classes, length(TREE_PALETTE_CLASSIFICATION)))]
  } else {
    palette <- TREE_PALETTE_REGRESSION
  }

  rpart.plot::rpart.plot(
    model,
    type = 4,
    extra = extra_val,
    under = TRUE,
    fallen.leaves = TRUE,
    roundint = FALSE,
    box.palette = palette,
    shadow.col = 0,
    border.col = TREE_NODE_BORDER,
    main = title
  )
}

#' Convert an rpart model to visNetwork nodes and edges
rpart_to_visnetwork <- function(model, data_dict = NULL) {
  frame <- model$frame
  node_ids <- as.integer(rownames(frame))

  is_classification <- model$method == "class"
  ylevels <- attr(model, "ylevels")
  split_labels <- labels(model, minlength = 0)

  # Build color mapping
  if (is_classification) {
    n_classes <- length(ylevels)
    pal <- TREE_PALETTE_CLASSIFICATION[seq_len(min(n_classes, length(TREE_PALETTE_CLASSIFICATION)))]
    class_colors <- setNames(pal, ylevels)
  } else {
    pred_vals <- frame$yval
    ramp <- grDevices::colorRampPalette(c("#deebf7", "#3182bd"))(100)
    val_range <- range(pred_vals)
    if (val_range[1] == val_range[2]) {
      scaled <- rep(50, length(pred_vals))
    } else {
      scaled <- round((pred_vals - val_range[1]) / (val_range[2] - val_range[1]) * 99) + 1
    }
  }

  # Helper: get data_dict label for a variable
  get_label <- function(varname) {
    if (!is.null(data_dict)) {
      idx <- match(varname, data_dict$variable)
      if (!is.na(idx)) {
        lbl <- data_dict$label[idx]
        if (nchar(lbl) > 40) lbl <- paste0(substr(lbl, 1, 37), "...")
        return(lbl)
      }
    }
    varname
  }

  nodes <- data.frame(
    id = integer(0),
    label = character(0),
    title = character(0),
    shape = character(0),
    color.background = character(0),
    color.border = character(0),
    font.color = character(0),
    stringsAsFactors = FALSE
  )

  edges <- data.frame(
    from = integer(0),
    to = integer(0),
    label = character(0),
    stringsAsFactors = FALSE
  )

  for (i in seq_along(node_ids)) {
    nid <- node_ids[i]
    is_leaf <- frame$var[i] == "<leaf>"
    n_obs <- frame$n[i]

    # Build tooltip
    if (is_classification) {
      class_counts <- frame$yval2[i, , drop = TRUE]
      # yval2 columns: predicted class, then counts per class, then proportions per class
      counts <- class_counts[1 + seq_len(n_classes)]
      probs <- class_counts[1 + n_classes + seq_len(n_classes)]
      dist_lines <- paste0(ylevels, ": ", counts, " (", round(probs * 100, 1), "%)")
      dist_html <- paste(dist_lines, collapse = "<br>")
      predicted <- ylevels[frame$yval[i]]
      tooltip <- paste0(
        "<b>", split_labels[i], "</b><br>",
        "n = ", n_obs, "<br><br>",
        "<b>Distribution:</b><br>", dist_html
      )
    } else {
      predicted <- round(frame$yval[i], 2)
      tooltip <- paste0(
        "<b>", split_labels[i], "</b><br>",
        "n = ", n_obs, "<br>",
        "Mean: ", predicted
      )
    }

    # Node color
    if (is_classification) {
      bg_color <- class_colors[ylevels[frame$yval[i]]]
    } else {
      bg_color <- ramp[scaled[i]]
    }

    # Node label and shape
    if (is_leaf) {
      if (is_classification) {
        node_label <- paste0(predicted, "\nn = ", n_obs)
      } else {
        node_label <- paste0(predicted, "\nn = ", n_obs)
      }
      shape <- "box"
    } else {
      var_label <- get_label(as.character(frame$var[i]))
      node_label <- var_label
      shape <- "ellipse"
    }

    nodes <- rbind(nodes, data.frame(
      id = nid,
      label = node_label,
      title = tooltip,
      shape = shape,
      color.background = unname(bg_color),
      color.border = TREE_NODE_BORDER,
      font.color = "#1a1a1a",
      stringsAsFactors = FALSE
    ))

    # Edges to children
    left_child <- nid * 2
    right_child <- nid * 2 + 1
    if (left_child %in% node_ids) {
      edges <- rbind(edges, data.frame(
        from = nid, to = left_child, label = "Yes",
        stringsAsFactors = FALSE
      ))
    }
    if (right_child %in% node_ids) {
      edges <- rbind(edges, data.frame(
        from = nid, to = right_child, label = "No",
        stringsAsFactors = FALSE
      ))
    }
  }

  list(nodes = nodes, edges = edges)
}
