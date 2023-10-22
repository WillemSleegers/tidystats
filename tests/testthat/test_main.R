# Setup -------------------------------------------------------------------

expected_statistics <- read_stats("../data/main.json")

tolerance <- 0.001

# add_stats() -------------------------------------------------------------

test_that("the t-test in main works", {
  sleep_wide <- reshape(
    sleep,
    direction = "wide",
    idvar = "ID",
    timevar = "group",
    sep = "_"
  )
  sleep_t_test <- t.test(sleep_wide$extra_1, sleep_wide$extra_2, paired = TRUE)

  statistics <- add_stats(list(), sleep_t_test, type = "primary")

  statistics$sleep_t_test$package <- NULL
  expected_statistics$sleep_t_test$package <- NULL

  expect_equal(
    object = statistics$sleep_t_test,
    expected = expected_statistics$sleep_t_test,
    tolerance = tolerance
  )
})

test_that("the linear regression in main works", {
  D9 <- tibble::tibble(
    group = gl(2, 10, 20, labels = c("Ctl", "Trt")),
    weight = c(
      4.17, 5.58, 5.18, 6.11, 4.50, 4.61, 5.17, 4.53, 5.33, 5.14, 4.81,
      4.17, 4.41, 3.59, 5.87, 3.83, 6.03, 4.89, 4.32, 4.69
    )
  )

  D9_lm <- lm(weight ~ group, data = D9)

  statistics <- add_stats(list(), D9_lm, preregistered = FALSE)

  statistics$D9_lm$package <- NULL
  expected_statistics$D9_lm$package <- NULL

  expect_equal(
    object = statistics$D9_lm,
    expected = expected_statistics$D9_lm,
    tolerance = tolerance
  )
})

test_that("the ANOVA in main works", {
  npk_aov <- aov(yield ~ block + N * P * K, npk)

  statistics <- add_stats(list(), npk_aov, notes = "An ANOVA example")

  statistics$npk_aov$package <- NULL
  expected_statistics$npk_aov$package <- NULL

  expect_equal(
    object = statistics$npk_aov,
    expected = expected_statistics$npk_aov,
    tolerance = tolerance
  )
})
