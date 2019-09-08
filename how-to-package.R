
# Load packages
library(devtools)
document()
install()

# README ------------------------------------------------------------------

knitr::knit("README.Rmd")

# Testing -----------------------------------------------------------------

# Test all tests
devtools::test()

# Test specific tests
library(testthat)
test_results <- read_stats("tests/testthat/test_results.csv")
test_file("tests/testthat/test-psych.R")

# CRAN submission ---------------------------------------------------------

# Check dependencies
usethis::use_package("dplyr")
usethis::use_package("tidyr")
usethis::use_package("purrr")
usethis::use_package("stringr")
usethis::use_package("readr")
usethis::use_package("tibble")
usethis::use_package("jsonlite")

# Check examples
devtools::run_examples()

# Check package
devtools::check()
devtools::check(args = c('--run-donttest')) # Without examples test

# run R CMD check on CRAN’s servers
devtools::check_win_release()

# Build tar
devtools::build()

# Add a data set ----------------------------------------------------------

usethis::use_data(quote_source, overwrite = TRUE)
