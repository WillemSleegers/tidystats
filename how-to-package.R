
# Update ------------------------------------------------------------------

# Document and install the package
devtools::document()
devtools::install()

# README ------------------------------------------------------------------

# Knit the README.Rmd file to a README.md file for Github
knitr::knit("README.Rmd")

# CRAN submission ---------------------------------------------------------

# Check examples
devtools::run_examples()

# Check package
devtools::check()
devtools::check(args = c('--run-donttest')) # Without examples test
devtools::check(args = c('--as-cran'))

# run R CMD check on CRAN’s servers
devtools::check_win_devel()
devtools::check_win_release()

# Build tar
devtools::build()

# Add a data set ----------------------------------------------------------

usethis::use_data(quote_source, overwrite = TRUE)

# Testing -----------------------------------------------------------------

# Test all tests
devtools::test()

# Test specific tests
library(testthat)
test_results <- read_stats("tests/testthat/test_results.csv")
test_file("tests/testthat/test-psych.R")

# Create a vignette -------------------------------------------------------

usethis::use_vignette("introduction-to-tidystats")

# Build website -----------------------------------------------------------

# Run once to configure package to use pkgdown
# usethis::use_pkgdown()

# Run to build the website
pkgdown::build_site()
