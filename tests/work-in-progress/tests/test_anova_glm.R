
# Setup -------------------------------------------------------------------

# Load test data
test_results <- read_stats(system.file("test_data/anova_glm.json", 
  package = "tidystats"))

# Set options
tolerance <- 0.001

# Test: anova.glm ---------------------------------------------------------

# Run models
counts <- c(18,17,15,20,10,20,25,13,12)
outcome <- gl(3,1,9)
treatment <- gl(3,3)
d.AD <- data.frame(treatment, outcome, counts)

glm.D93 <- glm(counts ~ outcome + treatment, family = poisson())
glm.D93a <- update(glm.D93, ~treatment * outcome)

test_that("anova.glm works", {
  model <- anova(glm.D93)
  
  tidy_model <- tidy_stats(model)
  tidy_model_test <- test_results$anova_glm
  
  tidy_model$package$version <- NULL
  tidy_model_test$package$version <- NULL
  
  expect_equal(tidy_model, tidy_model_test, tolerance = tolerance)
})

test_that("cp anova.glm works", {
  model <- anova(glm.D93, test = "Cp")
  
  tidy_model <- tidy_stats(model)
  tidy_model_test <- test_results$anova_glm_cp
  
  tidy_model$package$version <- NULL
  tidy_model_test$package$version <- NULL
  
  expect_equal(tidy_model, tidy_model_test, tolerance = tolerance)
})

test_that("chisq anova.glm works", {
  model <- anova(glm.D93, test = "Chisq")
  
  tidy_model <- tidy_stats(model)
  tidy_model_test <- test_results$anova_glm_chisq
  
  tidy_model$package$version <- NULL
  tidy_model_test$package$version <- NULL
  
  expect_equal(tidy_model, tidy_model_test, tolerance = tolerance)
})

test_that("rao anova.glm works", {
  model <- anova(glm.D93, glm.D93a, test = "Rao")
  
  tidy_model <- tidy_stats(model)
  tidy_model_test <- test_results$anova_glm_rao
  
  tidy_model$package$version <- NULL
  tidy_model_test$package$version <- NULL
  
  expect_equal(tidy_model, tidy_model_test, tolerance = tolerance)
})
