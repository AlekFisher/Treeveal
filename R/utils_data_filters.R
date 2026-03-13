# utils_data_filters.R
# Pure helpers for applying a single active dataset filter.

get_filterable_variables <- function(data) {
  if (is.null(data)) {
    return(character(0))
  }

  var_names <- names(data)
  keep <- vapply(data, function(column) {
    non_missing <- unique(stats::na.omit(column))
    length(non_missing) > 1 && (is.numeric(column) || is.factor(column) || is.character(column))
  }, logical(1))

  var_names[keep]
}

build_single_filter_spec <- function(data, variable, selected_values = NULL, range = NULL) {
  if (is.null(data) || !nzchar(variable) || !variable %in% names(data)) {
    return(NULL)
  }

  column <- data[[variable]]

  if (is.numeric(column)) {
    if (is.null(range) || length(range) != 2 || any(!is.finite(range))) {
      return(NULL)
    }

    list(
      variable = variable,
      type = "numeric",
      range = as.numeric(range)
    )
  } else if (is.factor(column) || is.character(column)) {
    selected_values <- unique(as.character(selected_values))
    selected_values <- selected_values[nzchar(selected_values)]

    if (length(selected_values) == 0) {
      return(NULL)
    }

    list(
      variable = variable,
      type = "categorical",
      values = selected_values
    )
  } else {
    NULL
  }
}

apply_single_filter <- function(data, filter_spec) {
  if (is.null(data) || is.null(filter_spec) || !is.list(filter_spec)) {
    return(data)
  }

  variable <- filter_spec$variable
  if (is.null(variable) || !variable %in% names(data)) {
    return(data)
  }

  column <- data[[variable]]

  if (identical(filter_spec$type, "numeric")) {
    range <- filter_spec$range
    if (is.null(range) || length(range) != 2) {
      return(data)
    }

    keep <- !is.na(column) & column >= range[1] & column <= range[2]
  } else if (identical(filter_spec$type, "categorical")) {
    values <- filter_spec$values
    if (is.null(values) || length(values) == 0) {
      return(data)
    }

    keep <- !is.na(column) & as.character(column) %in% values
  } else {
    return(data)
  }

  data[keep, , drop = FALSE]
}

format_filter_summary <- function(filter_spec) {
  if (is.null(filter_spec) || is.null(filter_spec$variable)) {
    return("No active filter")
  }

  if (identical(filter_spec$type, "numeric")) {
    range <- filter_spec$range
    paste0(filter_spec$variable, ": ", format(range[1]), " to ", format(range[2]))
  } else if (identical(filter_spec$type, "categorical")) {
    values <- filter_spec$values
    if (is.null(values)) {
      values <- character(0)
    }
    paste0(filter_spec$variable, ": ", paste(values, collapse = ", "))
  } else {
    "No active filter"
  }
}
