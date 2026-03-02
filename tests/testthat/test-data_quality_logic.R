# test-data_quality_logic.R
# Tests for detect_outliers() extracted from mod_data_quality.R

test_that("detect_outliers returns empty df for no numeric vars", {
  df <- data.frame(x = factor(c("a", "b")))
  result <- detect_outliers(df, character(0))
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 0)
})

test_that("detect_outliers returns correct columns", {
  df <- data.frame(x = rnorm(100))
  result <- detect_outliers(df, "x")
  expected_cols <- c("Variable", "Q1", "Q3", "IQR",
                     "N_Mild", "Pct_Mild", "N_Extreme", "Pct_Extreme", "Status")
  expect_equal(names(result), expected_cols)
})

test_that("detect_outliers reports OK for normal data", {
  set.seed(42)
  df <- data.frame(x = rnorm(500, mean = 50, sd = 5))
  result <- detect_outliers(df, "x")
  expect_equal(nrow(result), 1)
  expect_equal(result$Status, "OK")
})

test_that("detect_outliers detects extreme outliers", {
  # Build data where >5% are extreme outliers
  set.seed(42)
  n <- 100
  x <- c(rnorm(85, 50, 2), rep(200, 15))
  df <- data.frame(x = x)
  result <- detect_outliers(df, "x")
  expect_equal(result$Status, "Extreme")
})

test_that("detect_outliers detects mild outliers", {
  # Build data where mild+extreme > 5% but extreme alone <= 5%
  set.seed(42)
  n <- 200
  base <- rnorm(180, 50, 5)
  # Add values just outside 1.5*IQR but within 3*IQR
  q1 <- quantile(base, 0.25)
  q3 <- quantile(base, 0.75)
  iqr_val <- q3 - q1
  mild_outliers <- rep(q3 + 2 * iqr_val, 20)
  df <- data.frame(x = c(base, mild_outliers))
  result <- detect_outliers(df, "x")
  expect_true(result$Status %in% c("Mild", "Extreme"))
})

test_that("detect_outliers skips vars with < 4 observations", {
  df <- data.frame(x = c(1, 2, 3))
  result <- detect_outliers(df, "x")
  expect_equal(nrow(result), 0)
})

test_that("detect_outliers skips vars with IQR == 0", {
  df <- data.frame(x = c(rep(5, 50), 1, 100))
  result <- detect_outliers(df, "x")
  expect_equal(nrow(result), 0)
})

test_that("detect_outliers handles NAs gracefully", {
  set.seed(42)
  x <- c(rnorm(100, 50, 5), rep(NA, 20))
  df <- data.frame(x = x)
  result <- detect_outliers(df, "x")
  expect_equal(nrow(result), 1)
  # Percentages should be based on non-NA count
  expect_true(result$Pct_Mild >= 0)
})

test_that("detect_outliers works on readmission LOS (real demo data)", {
  readm <- generate_demo_dataset("readmission")
  numeric_vars <- names(readm$data)[sapply(readm$data, is.numeric)]
  result <- detect_outliers(readm$data, numeric_vars)
  expect_s3_class(result, "data.frame")
  expect_gt(nrow(result), 0)
  # LOS is right-skewed, should have some outliers
  los_row <- result[result$Variable == "LOS", ]
  expect_equal(nrow(los_row), 1)
})

test_that("detect_outliers handles multiple numeric vars", {
  set.seed(42)
  df <- data.frame(a = rnorm(100), b = rnorm(100), c = rnorm(100))
  result <- detect_outliers(df, c("a", "b", "c"))
  expect_equal(nrow(result), 3)
  expect_equal(result$Variable, c("a", "b", "c"))
})
