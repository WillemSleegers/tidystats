
# Setup -------------------------------------------------------------------

# Load test data
test_results <- read_stats(system.file("results.json", package = "tidystats"))

# Set options
tolerance <- 0.001

# Test: add_stats() -------------------------------------------------------

test_that("add_stats works", {
  # Conduct three different analyses
  # t-test:
  sleep_test <- t.test(extra ~ group, data = sleep, paired = TRUE)
  
  # lm:
  ctl <- c(4.17,5.58,5.18,6.11,4.50,4.61,5.17,4.53,5.33,5.14)
  trt <- c(4.81,4.17,4.41,3.59,5.87,3.83,6.03,4.89,4.32,4.69)
  group <- gl(2, 10, 20, labels = c("Ctl","Trt"))
  weight <- c(ctl, trt)
  lm_D9 <- lm(weight ~ group)
  
  # ANOVA:
  npk_aov <- aov(yield ~ block + N*P*K, npk)
  
  # Create an empty list
  results <- list()
  
  # Add the analyses to the empty list
  results <- results %>%
    add_stats(sleep_test, type = "primary") %>%
    add_stats(lm_D9, preregistered = FALSE) %>%
    add_stats(npk_aov, notes = "An ANOVA example")
  
  # Set versions to NULL
  results$sleep_test$package$version <- NULL
  results$lm_D9$package$version <- NULL
  results$npk_aov$package$version <- NULL
  
  test_results$sleep_test$package$version <- NULL
  test_results$lm_D9$package$version <- NULL
  test_results$npk_aov$package$version <- NULL
  
  expect_equal(results, test_results, tolerance = tolerance)
})