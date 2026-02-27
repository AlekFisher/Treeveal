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
