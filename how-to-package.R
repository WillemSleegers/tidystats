# Todos -------------------------------------------------------------------

# TODO: Call deviance dfs df numerator and df denominator?
# TODO: Store all sample estimates in the case of a 4-sample chi-squared test?
# TODO: Simplify the method name of Kolmogorov-Smirnov tests?
# TODO: Rename the name of degrees of freedom and related values to 'parameter'?
# TODO: Use the checkmate package
# TODO: Create a vignette to describe the development process of adding support
#       for an analysis
# TODO: Improve tidystats count function (redo the loop function)

# Load functions ----------------------------------------------------------

pkgload::load_all()

# Documentation -----------------------------------------------------------

# Run once
# usethis::use_roxygen_md()

# Update documentation
devtools::document()

# Inspect documentation
pkgload::dev_help("add_stats")

# Installation ------------------------------------------------------------

# Install the dev version
devtools::install()
# .rs.restartR()

# Add dependency ----------------------------------------------------------

# usethis::use_package("lavaan", "Suggests")

# Testing -----------------------------------------------------------------

# Add a test
# usethis::use_test("add_stats")

# Test all tests
devtools::test()
testthat::test_dir(path = "tests/testthat/tests/")

# Test a specific test
testthat::test_file("tests/testthat/test_irr.R")

# Create a vignette -------------------------------------------------------

# usethis::use_vignette("supported-functions")

# Add a data set ----------------------------------------------------------

# usethis::use_data(quote_source, overwrite = TRUE)

# Build website -----------------------------------------------------------

# Update documentation
devtools::document()

# Update README
knitr::knit(input = "README.Rmd")

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
devtools::check(args = c("--run-donttest")) # Without examples test
devtools::check(args = c("--as-cran"))

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
