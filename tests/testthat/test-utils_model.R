# test-utils_model.R

# --- Shared fixtures ---
hcp <- generate_demo_dataset("hcp")
tx <- generate_demo_dataset("treatment")

# --- build_decision_tree() ---
test_that("build_decision_tree produces classification model on HCP data", {
  model <- build_decision_tree(
    data = hcp$data,
    outcome_var = "High_Prescriber",
    predictors = c("Comfortable_Initiating", "Cost_is_Barrier", "Specialty"),
    cp = 0.01, minbucket = 20, maxdepth = 5
  )
  expect_s3_class(model, "rpart")
  expect_equal(model$method, "class")
})

test_that("build_decision_tree produces regression model on treatment data", {
  model <- build_decision_tree(
    data = tx$data,
    outcome_var = "Symptom_Improvement",
    predictors = c("Treatment_Group", "Baseline_Severity", "Duration_Weeks"),
    cp = 0.01, minbucket = 20, maxdepth = 5
  )
  expect_s3_class(model, "rpart")
  expect_equal(model$method, "anova")
})

test_that("build_decision_tree respects maxdepth parameter", {
  model <- build_decision_tree(
    data = hcp$data,
    outcome_var = "High_Prescriber",
    predictors = setdiff(names(hcp$data), "High_Prescriber"),
    cp = 0.001, minbucket = 5, maxdepth = 2
  )
  # Tree depth check: node IDs encode depth (root=1, children 2,3, grandchildren 4-7)
  node_ids <- as.integer(rownames(model$frame))
  max_depth_actual <- max(floor(log2(node_ids)))
  expect_lte(max_depth_actual, 2)
})

test_that("build_decision_tree has terminal nodes", {
  model <- build_decision_tree(
    data = hcp$data,
    outcome_var = "High_Prescriber",
    predictors = c("Comfortable_Initiating", "Cost_is_Barrier"),
    cp = 0.01, minbucket = 20, maxdepth = 5
  )
  n_leaves <- sum(model$frame$var == "<leaf>")
  expect_gte(n_leaves, 2)
})

# --- render_tree_plot() ---
test_that("render_tree_plot does not error for classification", {
  model <- build_decision_tree(
    data = hcp$data,
    outcome_var = "High_Prescriber",
    predictors = c("Comfortable_Initiating", "Cost_is_Barrier"),
    cp = 0.01, minbucket = 20, maxdepth = 4
  )
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp))
  png(tmp)
  expect_no_error(render_tree_plot(model))
  dev.off()
})

test_that("render_tree_plot does not error for regression", {
  model <- build_decision_tree(
    data = tx$data,
    outcome_var = "Symptom_Improvement",
    predictors = c("Treatment_Group", "Duration_Weeks"),
    cp = 0.01, minbucket = 20, maxdepth = 4
  )
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp))
  png(tmp)
  expect_no_error(render_tree_plot(model))
  dev.off()
})

# --- rpart_to_visnetwork() ---
test_that("rpart_to_visnetwork returns nodes and edges for classification", {
  model <- build_decision_tree(
    data = hcp$data,
    outcome_var = "High_Prescriber",
    predictors = c("Comfortable_Initiating", "Cost_is_Barrier"),
    cp = 0.01, minbucket = 20, maxdepth = 4
  )
  result <- rpart_to_visnetwork(model)
  expect_type(result, "list")
  expect_true(all(c("nodes", "edges") %in% names(result)))
  expect_s3_class(result$nodes, "data.frame")
  expect_s3_class(result$edges, "data.frame")
  expect_true("id" %in% names(result$nodes))
  expect_true(all(c("from", "to") %in% names(result$edges)))
})

test_that("rpart_to_visnetwork works for regression", {
  model <- build_decision_tree(
    data = tx$data,
    outcome_var = "Symptom_Improvement",
    predictors = c("Treatment_Group", "Duration_Weeks"),
    cp = 0.01, minbucket = 20, maxdepth = 4
  )
  result <- rpart_to_visnetwork(model)
  expect_s3_class(result$nodes, "data.frame")
  expect_gt(nrow(result$nodes), 0)
})

test_that("rpart_to_visnetwork applies data_dict labels", {
  model <- build_decision_tree(
    data = hcp$data,
    outcome_var = "High_Prescriber",
    predictors = c("Comfortable_Initiating", "Cost_is_Barrier"),
    cp = 0.01, minbucket = 20, maxdepth = 4
  )
  result_no_dict <- rpart_to_visnetwork(model, data_dict = NULL)
  result_with_dict <- rpart_to_visnetwork(model, data_dict = hcp$dict)

  # Internal (non-leaf) node labels should differ when dict is provided
  internal_no <- result_no_dict$nodes[result_no_dict$nodes$shape == "ellipse", "label"]
  internal_with <- result_with_dict$nodes[result_with_dict$nodes$shape == "ellipse", "label"]

  # At least one label should change when dict is applied
  if (length(internal_no) > 0 && length(internal_with) > 0) {
    expect_false(all(internal_no == internal_with))
  }
})

test_that("rpart_to_visnetwork nodes have required columns", {
  model <- build_decision_tree(
    data = hcp$data,
    outcome_var = "High_Prescriber",
    predictors = c("Comfortable_Initiating", "Cost_is_Barrier"),
    cp = 0.01, minbucket = 20, maxdepth = 4
  )
  result <- rpart_to_visnetwork(model)
  expected_cols <- c("id", "label", "title", "shape", "color.background", "color.border", "font.color")
  expect_true(all(expected_cols %in% names(result$nodes)))
})
