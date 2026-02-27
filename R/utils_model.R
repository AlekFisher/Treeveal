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
