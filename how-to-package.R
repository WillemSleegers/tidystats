# Todos -------------------------------------------------------------------

# TODO: Rename certain terms to contrasts?
# TODO: # Call deviance dfs df numerator and df denominator?
# TODO: Replace some of the statistics extraction code with loops over all
#      columns and then fix the names after

# Update ------------------------------------------------------------------

# Update documentation and (re)install the package
devtools::document()
devtools::install()
detach("package:tidystats", unload = TRUE)
library(tidystats)

# Restart sessions --------------------------------------------------------

.rs.restartR()
library(tidystats)

# Setup -------------------------------------------------------------------

# Create README file
usethis::use_readme_rmd(open = rlang::is_interactive())

# Create pkgdown website
usethis::use_pkgdown()

# Testing -----------------------------------------------------------------

# Add a test
# usethis::use_test("add_stats")

# Test all tests
devtools::test()

# Test specific tests
testthat::test_file("tests/testthat/test_glm.R")

# Create a vignette -------------------------------------------------------

# usethis::use_vignette("read-and-use-a-tidystats-file")

# Add a data set ----------------------------------------------------------

# usethis::use_data(quote_source, overwrite = TRUE)

# Build website -----------------------------------------------------------

# Run to build the website
pkgdown::build_site_github_pages()

# Preview the site
pkgdown::preview_site()

# Delete website files
# pkgdown::clean_site()

# CRAN submission ---------------------------------------------------------

# Update README
knitr::knit(input = "README.Rmd")

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

# Build tar
devtools::build()
