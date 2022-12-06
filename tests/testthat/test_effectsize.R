
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

test_that("effectsize's Cohen's d not pooled works", {
  model <- cohens_d(mpg ~ am, data = mtcars, pooled_sd = FALSE)
  
  expect_equal_models(
    model = model,
    expected_tidy_model = expected_statistics$cohens_d_not_pooled
  )
})

test_that("effectsize's Cohen's d mu works", {
  model <- cohens_d(mpg ~ am, data = mtcars, mu = -5)
  
  expect_equal_models(
    model = model,
    expected_tidy_model = expected_statistics$cohens_d_mu
  )
})

test_that("effectsize's Cohen's d less works", {
  model <- cohens_d(mpg ~ am, data = mtcars, alternative = "less")
  
  expect_equal_models(
    model = model,
    expected_tidy_model = expected_statistics$cohens_d_less
  )
})

test_that("effectsize's Cohen's d one sample works", {
  model <- cohens_d(wt ~ 1, data = mtcars)
  
  expect_equal_models(
    model = model,
    expected_tidy_model = expected_statistics$cohens_d_one_sample
  )
})

test_that("effectsize's Cohen's d paired works", {
  model <- cohens_d(Pair(extra[group == 1], extra[group == 2]) ~ 1, 
                    data = sleep)
  
  expect_equal_models(
    model = model,
    expected_tidy_model = expected_statistics$cohens_d_paired
  )
})

# hedges_g() --------------------------------------------------------------
test_that("effectsize's Hedges' g works", {
  model <- hedges_g(mpg ~ am, data = mtcars)
  
  expect_equal_models(
    model = model,
    expected_tidy_model = expected_statistics$hedges_g
  )
})

test_that("effectsize's Hedges' g not pooled works", {
  model <- hedges_g(mpg ~ am, data = mtcars, pooled_sd = FALSE)
  
  expect_equal_models(
    model = model,
    expected_tidy_model = expected_statistics$hedges_g_not_pooled
  )
})

test_that("effectsize's Hedges' g mu works", {
  model <- hedges_g(mpg ~ am, data = mtcars, mu = -5)
  
  expect_equal_models(
    model = model,
    expected_tidy_model = expected_statistics$hedges_g_mu
  )
})

test_that("effectsize's Hedges' g less works", {
  model <- hedges_g(mpg ~ am, data = mtcars, alternative = "less")
  
  expect_equal_models(
    model = model,
    expected_tidy_model = expected_statistics$hedges_g_less
  )
})

test_that("effectsize's Hedges' g one sample works", {
  model <- hedges_g(wt ~ 1, data = mtcars)
  
  expect_equal_models(
    model = model,
    expected_tidy_model = expected_statistics$hedges_g_one_sample
  )
})

test_that("effectsize's Hedges' g one sample works", {
  model <- hedges_g(Pair(extra[group == 1], extra[group == 2]) ~ 1, data = sleep)
  
  expect_equal_models(
    model = model,
    expected_tidy_model = expected_statistics$hedges_g_paired
  )
})

# glass_delta() --------------------------------------------------------------
test_that("effectsize's Glass's delta works", {
  model <- glass_delta(mpg ~ am, data = mtcars)
  
  expect_equal_models(
    model = model,
    expected_tidy_model = expected_statistics$glass_delta
  )
})

test_that("effectsize's Glass's delta mu works", {
  model <- glass_delta(mpg ~ am, data = mtcars, mu = -5) 
  
  expect_equal_models(
    model = model,
    expected_tidy_model = expected_statistics$glass_delta_mu
  )
})

test_that("effectsize's Glass's delta less works", {
  model <- glass_delta(mpg ~ am, data = mtcars, alternative = "less")
  
  expect_equal_models(
    model = model,
    expected_tidy_model = expected_statistics$glass_delta_less
  )
})

