
# Setup -------------------------------------------------------------------

# Load test data
path <- system.file(
  "tests/testthat/data/effectsize.json", 
  package = "tidystats"
)
expected_statistics <- read_stats(path)

# cohens_d() --------------------------------------------------------------

test_that("effectsize's Cohen's d works", {
  model <- cohens_d(mpg ~ am, data = mtcars)
  
  expect_equal_models(
    model = model, 
    expected_tidy_model = expected_statistics$cohens_d
  )
})
