# Run all unit tests
# Usage: Rscript tests/testthat.R (from project root)
# Or:    testthat::test_dir("tests/testthat") from an R session
library(testthat)
test_dir("tests/testthat")
