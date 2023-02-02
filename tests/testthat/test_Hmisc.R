
# Setup -------------------------------------------------------------------

# Load test data
path <- system.file("tests/data/Hmisc.json", package = "tidystats")
expected_statistics <- read_stats(path)

# aov() -------------------------------------------------------------------

test_that("Hmisc's rcorr works", {
  x <- c(-2, -1, 0, 1, 2)
  y <- c(4, 1, 0, 1, 4)
  z <- c(1, 2, 3, 4, NA)
  v <- c(1, 2, 3, 4, 5)

  model <- rcorr(cbind(x, y, z, v))
  
  expect_equal_models(
    model = model, 
    expected_tidy_model = expected_statistics$rcorr
  )
})
