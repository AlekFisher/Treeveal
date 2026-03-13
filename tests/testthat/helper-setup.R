# helper-setup.R
# Auto-loaded before all test files by testthat

library(rpart)
library(rpart.plot)
library(dplyr)
library(ggplot2)

# Source all utils and module files needed for testing
r_dir <- file.path(testthat::test_path(), "..", "..", "R")

source(file.path(r_dir, "utils_theme.R"))
source(file.path(r_dir, "utils_ai_providers.R"))
source(file.path(r_dir, "utils_data.R"))
source(file.path(r_dir, "utils_data_filters.R"))
source(file.path(r_dir, "utils_demo_data.R"))
source(file.path(r_dir, "utils_model.R"))
source(file.path(r_dir, "mod_export.R"))
