# test-utils_data.R

# --- load_data_file() ---
test_that("load_data_file loads CSV with factors", {
  tmp <- tempfile(fileext = ".csv")
  on.exit(unlink(tmp))
  write.csv(data.frame(x = c("a", "b", "a"), y = 1:3), tmp, row.names = FALSE)

  result <- load_data_file(tmp, "test.csv")
  expect_s3_class(result, "data.frame")
  expect_true(is.factor(result$x))
  expect_equal(nrow(result), 3)
})

test_that("load_data_file loads RDS", {
  tmp <- tempfile(fileext = ".rds")
  on.exit(unlink(tmp))
  saveRDS(data.frame(a = 1:5, b = letters[1:5]), tmp)

  result <- load_data_file(tmp, "test.rds")
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 5)
  expect_true(is.factor(result$b))
})

test_that("load_data_file errors on unsupported type", {
  expect_error(load_data_file("fake.path", "data.json"), "Unsupported file type")
})

test_that("load_data_file errors on non-df RDS", {
  tmp <- tempfile(fileext = ".rds")
  on.exit(unlink(tmp))
  saveRDS(list(a = 1, b = 2), tmp)

  expect_error(load_data_file(tmp, "test.rds"), "must contain a data frame")
})

# --- load_dict_file() ---
test_that("load_dict_file works with standard column names", {
  tmp <- tempfile(fileext = ".csv")
  on.exit(unlink(tmp))
  write.csv(data.frame(variable = c("x", "y"), label = c("X var", "Y var")),
            tmp, row.names = FALSE)

  result <- load_dict_file(tmp, "dict.csv")
  expect_equal(nrow(result), 2)
  expect_true(all(c("variable", "label", "notes") %in% names(result)))
})

test_that("load_dict_file maps alternative column names", {
  tmp <- tempfile(fileext = ".csv")
  on.exit(unlink(tmp))
  write.csv(data.frame(var = c("x"), description = c("The X variable")),
            tmp, row.names = FALSE)

  result <- load_dict_file(tmp, "dict.csv")
  expect_equal(result$variable, "x")
  expect_equal(result$label, "The X variable")
})

test_that("load_dict_file errors when required columns missing", {
  tmp <- tempfile(fileext = ".csv")
  on.exit(unlink(tmp))
  write.csv(data.frame(foo = 1, bar = 2), tmp, row.names = FALSE)

  expect_error(load_dict_file(tmp, "dict.csv"), "variable")
})

test_that("load_dict_file filters blank rows", {
  tmp <- tempfile(fileext = ".csv")
  on.exit(unlink(tmp))
  write.csv(data.frame(variable = c("x", "", "y"), label = c("X", "", "Y")),
            tmp, row.names = FALSE)

  result <- load_dict_file(tmp, "dict.csv")
  expect_equal(nrow(result), 2)
  expect_equal(result$variable, c("x", "y"))
})

test_that("load_dict_file auto-adds notes column", {
  tmp <- tempfile(fileext = ".csv")
  on.exit(unlink(tmp))
  write.csv(data.frame(variable = "x", label = "X var"), tmp, row.names = FALSE)

  result <- load_dict_file(tmp, "dict.csv")
  expect_true("notes" %in% names(result))
  expect_true(is.na(result$notes))
})

# --- convert_binary_to_factor() ---
test_that("convert_binary_to_factor converts 0/1 columns", {
  df <- data.frame(a = c(0, 1, 0, 1), b = c(10, 20, 30, 40))
  result <- convert_binary_to_factor(df, c("a", "b"))
  expect_true(is.factor(result$a))
  expect_equal(levels(result$a), c("0", "1"))
  expect_true(is.numeric(result$b))
})

test_that("convert_binary_to_factor skips non-binary numeric", {
  df <- data.frame(x = c(1, 2, 3))
  result <- convert_binary_to_factor(df, "x")
  expect_true(is.numeric(result$x))
})

test_that("convert_binary_to_factor skips existing factors", {
  df <- data.frame(x = factor(c("a", "b")))
  result <- convert_binary_to_factor(df, "x")
  expect_true(is.factor(result$x))
  expect_equal(levels(result$x), c("a", "b"))
})

test_that("convert_binary_to_factor handles NAs in binary column", {
  df <- data.frame(a = c(0, 1, NA, 0))
  result <- convert_binary_to_factor(df, "a")
  expect_true(is.factor(result$a))
  expect_true(is.na(result$a[3]))
})

test_that("convert_binary_to_factor ignores missing variable names", {
  df <- data.frame(x = c(0, 1))
  result <- convert_binary_to_factor(df, c("x", "nonexistent"))
  expect_true(is.factor(result$x))
})
