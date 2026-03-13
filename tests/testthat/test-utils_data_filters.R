# test-utils_data_filters.R

test_that("get_filterable_variables returns supported columns with more than one value", {
  df <- data.frame(
    segment = factor(c("A", "B", "A")),
    score = c(10, 20, 30),
    constant = c("x", "x", "x"),
    flag = c(TRUE, TRUE, FALSE)
  )

  expect_equal(get_filterable_variables(df), c("segment", "score"))
})

test_that("apply_single_filter subsets categorical values", {
  df <- data.frame(
    segment = c("A", "B", "A", "C"),
    score = c(1, 2, 3, 4),
    stringsAsFactors = FALSE
  )

  filter_spec <- build_single_filter_spec(df, "segment", selected_values = c("A", "C"))
  filtered <- apply_single_filter(df, filter_spec)

  expect_equal(filtered$segment, c("A", "A", "C"))
  expect_equal(nrow(filtered), 3)
})

test_that("apply_single_filter subsets numeric ranges", {
  df <- data.frame(
    segment = c("A", "B", "A", "C"),
    score = c(1, 2, 3, 4),
    stringsAsFactors = FALSE
  )

  filter_spec <- build_single_filter_spec(df, "score", range = c(2, 3))
  filtered <- apply_single_filter(df, filter_spec)

  expect_equal(filtered$score, c(2, 3))
  expect_equal(nrow(filtered), 2)
})
