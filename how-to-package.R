# Todos -------------------------------------------------------------------

# TODO: Call deviance dfs df numerator and df denominator?
# TODO: Store all sample estimates in the case of a 4-sample chi-squared test?
# TODO: Simplify the method name of Kolmogorov-Smirnov tests?
# TODO: Rename the name of degrees of freedom and related values to 'parameter'?
# TODO: Create a vignette to describe the development process of adding support
#       for an analysis
# TODO: Improve tidystats count function (redo the loop function)
# TODO: Tighten tolerance on mood.test() z and p tests once R release catches up
#       to the bug fix in R devel (r89561) that corrected the tie-correction
#       formula: (N - t)^2 → (N - 2 * cumsum(t) + t)^2 per Mielke (1967)

# Load functions ----------------------------------------------------------

pkgload::load_all()

# Documentation -----------------------------------------------------------

# Run once
# usethis::use_roxygen_md()

# Update documentation
devtools::document()

# Inspect documentation
pkgload::dev_help("add_stats")
pkgload::dev_help("describe_data")

# Installation ------------------------------------------------------------

# Install the dev version
devtools::install()

# Add dependency ----------------------------------------------------------

# usethis::use_package("lavaan", "Suggests")

# Testing -----------------------------------------------------------------

# Add a test
# usethis::use_test("add_stats")

# Test all tests
devtools::test()
testthat::test_dir(path = "tests/testthat/")

# Test a specific test
testthat::test_file("tests/testthat/test_Hmisc.R")

# Create a vignette -------------------------------------------------------

# usethis::use_vignette("supported-functions")

# Add a data set ----------------------------------------------------------

# usethis::use_data(quote_source, overwrite = TRUE)

# Build website -----------------------------------------------------------

# Update documentation
devtools::document()

# Update README
#devtools::build_readme()
knitr::knit(input = "README.Rmd")

# Run to build the website
pkgdown::build_site_github_pages()
pkgdown::build_articles()

# Preview the site
pkgdown::preview_site()

# Delete website files
pkgdown::clean_site()

# CRAN submission ---------------------------------------------------------

# Update README
knitr::knit(input = "README.Rmd")

# Update website
pkgdown::build_site_github_pages()

# Spellcheck
devtools::spell_check()

# Check examples
devtools::run_examples()

# CRAN comments
usethis::use_cran_comments()

# Check tests
devtools::test()

# Check package
# devtools::load_all()
devtools::check()
devtools::check(args = c("--no-tests")) # Without tests
devtools::check(args = c("--as-cran"))

# run R CMD check on CRAN’s servers
devtools::check_win_devel()
devtools::check_win_release()

# R-hub (v2: run rhub::rhub_setup() once first)
rhub::rhub_check()

# Submit
devtools::release()

# Setup -------------------------------------------------------------------

# Create README file
usethis::use_readme_rmd(open = interactive())

# Create pkgdown website
usethis::use_pkgdown()
