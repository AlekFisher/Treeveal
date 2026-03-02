# test-utils_demo_data.R

# --- get_demo_datasets() ---
test_that("get_demo_datasets returns list of 3 datasets", {
  datasets <- get_demo_datasets()
  expect_type(datasets, "list")
  expect_length(datasets, 3)
  expect_named(datasets, c("hcp", "readmission", "treatment"))
})

test_that("get_demo_datasets entries have expected metadata keys", {
  datasets <- get_demo_datasets()
  expected_keys <- c("name", "description", "n_obs", "n_vars", "type")
  for (nm in names(datasets)) {
    expect_true(
      all(expected_keys %in% names(datasets[[nm]])),
      info = paste("Missing keys in", nm)
    )
  }
})

test_that("get_demo_datasets type values are valid", {
  datasets <- get_demo_datasets()
  types <- sapply(datasets, `[[`, "type")
  expect_true(all(types %in% c("Classification", "Regression")))
})

# --- generate_demo_dataset() dispatcher ---
test_that("generate_demo_dataset dispatches for all known names", {
  for (nm in c("hcp", "readmission", "treatment")) {
    result <- generate_demo_dataset(nm)
    expect_type(result, "list")
    expect_true(all(c("data", "dict") %in% names(result)), info = nm)
  }
})

test_that("generate_demo_dataset errors on unknown name", {
  expect_error(generate_demo_dataset("unknown"), "Unknown demo dataset")
})

# --- HCP dataset ---
test_that("HCP dataset has correct dimensions", {
  hcp <- generate_demo_dataset("hcp")
  expect_equal(nrow(hcp$data), 500)
  expect_equal(ncol(hcp$data), 24)
})

test_that("HCP outcome is a factor", {
  hcp <- generate_demo_dataset("hcp")
  expect_true(is.factor(hcp$data$High_Prescriber))
  expect_equal(levels(hcp$data$High_Prescriber), c("Low", "High"))
})

test_that("HCP dictionary covers all data columns", {
  hcp <- generate_demo_dataset("hcp")
  expect_true(all(names(hcp$data) %in% hcp$dict$variable))
})

test_that("HCP output is deterministic (fixed seed)", {
  hcp1 <- generate_demo_dataset("hcp")
  hcp2 <- generate_demo_dataset("hcp")
  expect_identical(hcp1$data, hcp2$data)
})

# --- Readmission dataset ---
test_that("Readmission dataset has correct dimensions", {
  readm <- generate_demo_dataset("readmission")
  expect_equal(nrow(readm$data), 400)
  expect_equal(ncol(readm$data), 13)
})

test_that("Readmission outcome is a factor", {
  readm <- generate_demo_dataset("readmission")
  expect_true(is.factor(readm$data$Readmitted))
  expect_equal(levels(readm$data$Readmitted), c("No", "Yes"))
})

test_that("Readmission dictionary covers all data columns", {
  readm <- generate_demo_dataset("readmission")
  expect_true(all(names(readm$data) %in% readm$dict$variable))
})

# --- Treatment dataset ---
test_that("Treatment dataset has correct dimensions", {
  tx <- generate_demo_dataset("treatment")
  expect_equal(nrow(tx$data), 300)
  expect_equal(ncol(tx$data), 12)
})

test_that("Treatment outcome is numeric in [0, 100]", {
  tx <- generate_demo_dataset("treatment")
  expect_true(is.numeric(tx$data$Symptom_Improvement))
  expect_true(all(tx$data$Symptom_Improvement >= 0))
  expect_true(all(tx$data$Symptom_Improvement <= 100))
})

test_that("Treatment dictionary covers all data columns", {
  tx <- generate_demo_dataset("treatment")
  expect_true(all(names(tx$data) %in% tx$dict$variable))
})
