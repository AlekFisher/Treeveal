# utils_data.R
# Pure functions for data loading and transformation

#' Load a data file based on its extension
load_data_file <- function(file_path, file_name) {
  file_ext <- tolower(tools::file_ext(file_name))

  data <- switch(file_ext,
    "csv" = read.csv(file_path, stringsAsFactors = TRUE),
    "xlsx" = {
      df <- readxl::read_excel(file_path)
      df <- as.data.frame(df)
      df[] <- lapply(df, function(x) if (is.character(x)) as.factor(x) else x)
      df
    },
    "xls" = {
      df <- readxl::read_excel(file_path)
      df <- as.data.frame(df)
      df[] <- lapply(df, function(x) if (is.character(x)) as.factor(x) else x)
      df
    },
    "sav" = {
      df <- haven::read_sav(file_path)
      df <- as.data.frame(df)
      df[] <- lapply(df, function(x) {
        if (haven::is.labelled(x)) {
          as.factor(haven::as_factor(x))
        } else if (is.character(x)) {
          as.factor(x)
        } else {
          x
        }
      })
      df
    },
    "rds" = {
      df <- readRDS(file_path)
      if (!is.data.frame(df)) stop("RDS file must contain a data frame")
      df[] <- lapply(df, function(x) if (is.character(x)) as.factor(x) else x)
      df
    },
    "rdata" = ,
    "rda" = {
      temp_env <- new.env()
      load(file_path, envir = temp_env)
      objects <- ls(temp_env)
      df <- NULL
      for (obj_name in objects) {
        obj <- get(obj_name, envir = temp_env)
        if (is.data.frame(obj)) {
          df <- obj
          break
        }
      }
      if (is.null(df)) stop("No data frame found in RData file")
      df[] <- lapply(df, function(x) if (is.character(x)) as.factor(x) else x)
      df
    },
    stop(paste("Unsupported file type:", file_ext))
  )

  data
}

#' Load a data dictionary file
load_dict_file <- function(file_path, file_name) {
  file_ext <- tolower(tools::file_ext(file_name))

  dict <- switch(file_ext,
    "csv" = read.csv(file_path, stringsAsFactors = FALSE),
    "xlsx" = as.data.frame(readxl::read_excel(file_path)),
    "xls" = as.data.frame(readxl::read_excel(file_path)),
    stop(paste("Unsupported dictionary file type:", file_ext))
  )

  # Standardize column names (flexible matching)
  names(dict) <- tolower(names(dict))

  # Map common column name variations
  col_mapping <- list(
    variable = c("variable", "var", "var_name", "varname", "name", "column", "col"),
    label = c("label", "short_label", "short_label_raw", "description", "desc", "var_label", "varlabel"),
    notes = c("notes", "note", "comments", "comment", "details", "detail", "context", "info")
  )

  # Find and rename columns
  final_names <- names(dict)
  for (target in names(col_mapping)) {
    for (i in seq_along(final_names)) {
      if (final_names[i] %in% col_mapping[[target]]) {
        final_names[i] <- target
        break
      }
    }
  }
  names(dict) <- final_names

  # Validate required columns
  if (!"variable" %in% names(dict)) {
    stop("Dictionary must have a 'variable' column (or: var, var_name, name, column)")
  }
  if (!"label" %in% names(dict)) {
    stop("Dictionary must have a 'label' column (or: description, desc, short_label)")
  }

  # Add notes column if missing
  if (!"notes" %in% names(dict)) {
    dict$notes <- NA_character_
  }

  # Keep only relevant columns
  dict <- dict[, c("variable", "label", "notes")]

  # Remove rows with missing variable names
  dict <- dict[!is.na(dict$variable) & dict$variable != "", ]

  dict
}

#' Detect outliers in numeric variables using the IQR method
detect_outliers <- function(df, numeric_vars) {
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

  outlier_info
}

#' Convert binary numeric variables (0/1) to factors
convert_binary_to_factor <- function(df, vars) {
  for (var in vars) {
    if (var %in% names(df)) {
      col <- df[[var]]
      if (is.numeric(col) && !is.factor(col)) {
        unique_vals <- unique(na.omit(col))
        if (length(unique_vals) == 2 && all(unique_vals %in% c(0, 1))) {
          df[[var]] <- factor(col, levels = c(0, 1), labels = c("0", "1"))
        }
      }
    }
  }
  df
}
