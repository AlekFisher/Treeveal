# test-export_helpers.R
# Tests for pure helper functions in mod_export.R

# --- Shared fixtures ---
hcp <- generate_demo_dataset("hcp")
tx <- generate_demo_dataset("treatment")

cls_model <- build_decision_tree(
  data = hcp$data,
  outcome_var = "High_Prescriber",
  predictors = c("Comfortable_Initiating", "Cost_is_Barrier", "Specialty"),
  cp = 0.01, minbucket = 20, maxdepth = 4
)

reg_model <- build_decision_tree(
  data = tx$data,
  outcome_var = "Symptom_Improvement",
  predictors = c("Treatment_Group", "Duration_Weeks", "Baseline_Severity"),
  cp = 0.01, minbucket = 20, maxdepth = 4
)

# --- get_model_stats_text() ---
test_that("get_model_stats_text contains Accuracy for classification", {
  text <- get_model_stats_text(cls_model, hcp$data, "High_Prescriber",
                               cp = 0.01, minbucket = 20, maxdepth = 4)
  expect_true(grepl("Accuracy", text))
  expect_true(grepl("Terminal Nodes", text))
})

test_that("get_model_stats_text contains Terminal Nodes for regression", {
  text <- get_model_stats_text(reg_model, tx$data, "Symptom_Improvement",
                               cp = 0.01, minbucket = 20, maxdepth = 4)
  expect_true(grepl("Terminal Nodes", text))
  expect_false(grepl("Accuracy", text))
})

test_that("get_model_stats_text includes parameter values", {
  text <- get_model_stats_text(cls_model, hcp$data, "High_Prescriber",
                               cp = 0.02, minbucket = 15, maxdepth = 3)
  expect_true(grepl("0.02", text))
  expect_true(grepl("15", text))
  expect_true(grepl("3", text))
})

# --- get_decision_rules_text() ---
test_that("get_decision_rules_text returns non-empty string", {
  text <- get_decision_rules_text(cls_model)
  expect_true(nchar(text) > 0)
})

test_that("get_decision_rules_text respects max_lines", {
  full <- get_decision_rules_text(cls_model, max_lines = 1000)
  short <- get_decision_rules_text(cls_model, max_lines = 2)
  expect_lte(length(strsplit(short, "\n")[[1]]),
             length(strsplit(full, "\n")[[1]]))
})

# --- render_tree_to_file() ---
test_that("render_tree_to_file produces a file on disk", {
  path <- render_tree_to_file(cls_model, "High_Prescriber")
  on.exit(unlink(path))
  expect_true(file.exists(path))
  expect_true(file.info(path)$size > 0)
})

# --- render_importance_to_file() ---
test_that("render_importance_to_file produces a file when importance exists", {
  path <- render_importance_to_file(cls_model, data_dict = hcp$dict)
  if (!is.null(path)) {
    on.exit(unlink(path))
    expect_true(file.exists(path))
    expect_true(file.info(path)$size > 0)
  }
})

test_that("render_importance_to_file returns NULL when no importance", {
  # A stump model (single root node) may have no variable.importance
  stump <- build_decision_tree(
    data = hcp$data,
    outcome_var = "High_Prescriber",
    predictors = c("Comfortable_Initiating"),
    cp = 1, minbucket = 500, maxdepth = 1
  )
  # If the model is a stump with no splits, variable.importance is NULL
  if (is.null(stump$variable.importance)) {
    result <- render_importance_to_file(stump)
    expect_null(result)
  } else {
    skip("Model produced splits — cannot test NULL importance path")
  }
})

test_that("generate_analysis_script includes filter and model settings", {
  script <- generate_analysis_script(
    data_source = list(type = "file", filename = "analysis_input.csv"),
    active_filter = list(
      variable = "Specialty",
      type = "categorical",
      values = c("PCP", "Endocrinology")
    ),
    outcome_var = "High_Prescriber",
    predictors = c("Comfortable_Initiating", "Cost_is_Barrier"),
    cp = 0.02,
    minbucket = 15,
    maxdepth = 5,
    applied_factor_levels = list(Specialty = c("PCP", "Endocrinology", "Cardiology"))
  )

  expect_true(grepl("analysis_input.csv", script, fixed = TRUE))
  expect_true(grepl("outcome_var <- \"High_Prescriber\"", script, fixed = TRUE))
  expect_true(grepl("predictors <- c(\"Comfortable_Initiating\", \"Cost_is_Barrier\")", script, fixed = TRUE))
  expect_true(grepl("variable = \"Specialty\"", script, fixed = TRUE))
  expect_true(grepl("values = c(\"PCP\", \"Endocrinology\")", script, fixed = TRUE))
  expect_true(grepl("cp <- 0.02", script, fixed = TRUE))
  expect_true(grepl("data_raw$Specialty <- factor", script, fixed = TRUE))
})

test_that("generate_analysis_script uses demo loader for demo datasets", {
  script <- generate_analysis_script(
    data_source = list(type = "demo", demo_id = "hcp", label = "HCP GLP-1 Prescribing Survey"),
    active_filter = NULL,
    outcome_var = "High_Prescriber",
    predictors = c("Comfortable_Initiating"),
    cp = 0.01,
    minbucket = 20,
    maxdepth = 4
  )

  expect_true(grepl("source(\"R/utils_demo_data.R\")", script, fixed = TRUE))
  expect_true(grepl("generate_demo_dataset(\"hcp\")", script, fixed = TRUE))
  expect_true(grepl("active_filter <- NULL", script, fixed = TRUE))
})
