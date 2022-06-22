
# Setup -------------------------------------------------------------------

# Load test data
path <- system.file(
  "tests/testthat/data/pairwise_htest.json", 
  package = "tidystats"
)

test_results <- read_stats(path)

# Test: pairwise.t.test() -------------------------------------------------

test_that("pairwise t-tests with pooled SD works", {
  Month <- factor(airquality$Month, labels = month.abb[5:9])
  
  model <- pairwise.t.test(airquality$Ozone, Month)
  
  expect_equal_models(
    model = model, 
    tidy_model_test = test_results$pairwise_t_test
  )
})

test_that("Pairwise t tests with non-pooled SD works", {
  model <- pairwise.t.test(
    airquality$Ozone,
    Month,
    p.adjust.method = "bonf",
    pool.sd = FALSE
  )
  
  expect_equal_models(
    model = model, 
    tidy_model_test = test_results$pairwise_t_test_nonpooled
  )
})

test_that("pairwise paired t-tests works", {
  model <- pairwise.t.test(
    c(1, 2, 3, 1, 2, 4), 
    c(1, 1, 2, 2, 3, 3), 
    paired = TRUE
  )
  
  expect_equal_models(
    model = model, 
    tidy_model_test = test_results$pairwise_t_test_paired
  )
})

# Test: pairwise.prop.test() ----------------------------------------------

test_that("pairwise comparison of proportions works", {
  smokers <- c(83, 90, 129, 70)
  patients <- c(86, 93, 136, 82)
  
  model <- suppressWarnings(pairwise.prop.test(smokers, patients))

  expect_equal_models(
    model = model, 
    tidy_model_test = test_results$pairwise_prop_test
  )
})

# Test: pairwise.wilcox.test() --------------------------------------------

test_that("pairwise Wilcoxon rank sum exact test works", {
  model <- pairwise.wilcox.test(
    c(1, 2, 3, 4, 5, 6, 7, 8, 9, 11), 
    c(1, 1, 1, 1, 1, 2, 2, 2, 2, 2)
  )
  
  expect_equal_models(
    model = model, 
    tidy_model_test = test_results$pairwise_wilcox_test
  )
})

test_that("pairwise Wilcoxon signed rank exact test works", {
  model <- pairwise.wilcox.test(
    PlantGrowth$weight, 
    PlantGrowth$group,
    p.adjust.method = "BH", 
    paired = TRUE
  )
  
  expect_equal_models(
    model = model, 
    tidy_model_test = test_results$pairwise_wilcox_test_paired
  )
})
