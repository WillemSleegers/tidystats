
# Todos -------------------------------------------------------------------

#TODO: Rename certain terms to contrasts?
#TODO: # Call deviance dfs df numerator and df denominator?
#TODO: Replace some of the statistics extraction code with loops over all 
#      columns and then fix the names after
#TODO: Use pipes when adding statistics via add_statistic()?
#TODO: Store all sample estimates in the case of a 4-sample chi-squared test?
#TODO: Simplify the method name of Kolmogorov-Smirnov tests?
#TODO: Rename the name of degrees of freedom and related values to 'parameter'?

# Update ------------------------------------------------------------------

# Update documentation
devtools::document()

# Load the package
devtools::load_all()

# Installation ------------------------------------------------------------

# Install the dev version
devtools::install()
#.rs.restartR()

# Add dependency ----------------------------------------------------------

usethis::use_package("lavaan", "Suggests")

# Testing -----------------------------------------------------------------

# Add a test
# usethis::use_test("add_stats")

# Test a specific test
testthat::test_file("tests/testthat/test_main.R")

# Test all tests
devtools::test()
testthat::test_dir(path = "tests/testthat/tests/")

# Create a vignette -------------------------------------------------------

usethis::use_vignette("tidystats-development-adding-new-analyses")

# Add a data set ----------------------------------------------------------

# usethis::use_data(quote_source, overwrite = TRUE)

# README ------------------------------------------------------------------

# Update README
knitr::knit(input = "README.Rmd")

# Build website -----------------------------------------------------------

# Run to build the website
pkgdown::build_site_github_pages()

# Preview the site
pkgdown::preview_site()

# Delete website files
pkgdown::clean_site()

# CRAN submission ---------------------------------------------------------

# Update README
knitr::knit(input = "README.Rmd")

# Update website
pkgdown::build_site_github_pages()

# Check examples
devtools::run_examples()

# Check tests
devtools::test()

# Check package
# devtools::load_all()
devtools::check()
devtools::check(args = c('--run-donttest')) # Without examples test
devtools::check(args = c('--as-cran'))

# run R CMD check on CRAN’s servers
devtools::check_win_devel()
devtools::check_win_release()

# Submit
devtools::release()

# Setup -------------------------------------------------------------------

# Create README file
usethis::use_readme_rmd(open = rlang::is_interactive())

# Create pkgdown website
usethis::use_pkgdown()