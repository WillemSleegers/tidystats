# tidy_stats_to_data_frame() ----------------------------------------------

test_that("tidy stats to data frame works", {
  # Build the statistics list
  sleep_wide <- reshape(
    sleep,
    direction = "wide",
    idvar = "ID",
    timevar = "group",
    sep = "_"
  )
  sleep_t_test <- t.test(sleep_wide$extra_1, sleep_wide$extra_2, paired = TRUE)

  D9 <- data.frame(
    group = gl(2, 10, 20, labels = c("Ctl", "Trt")),
    weight = c(
      4.17, 5.58, 5.18, 6.11, 4.50, 4.61, 5.17, 4.53, 5.33, 5.14, 4.81,
      4.17, 4.41, 3.59, 5.87, 3.83, 6.03, 4.89, 4.32, 4.69
    )
  )
  D9_lm <- lm(weight ~ group, data = D9)
  npk_aov <- aov(yield ~ block + N * P * K, npk)

  statistics <- list()
  statistics <- add_stats(statistics, sleep_t_test, type = "primary")
  statistics <- add_stats(statistics, D9_lm, preregistered = FALSE)
  statistics <- add_stats(statistics, npk_aov, notes = "An ANOVA example")

  df <- tidy_stats_to_data_frame(statistics)
  rownames(df) <- NULL

  # Check structure
  expect_equal(nrow(df), 67)
  expect_true("identifier" %in% names(df))
  expect_true("statistic_name" %in% names(df))
  expect_true("value" %in% names(df))

  # Check t-test rows
  t_rows <- df[df$identifier == "sleep_t_test", ]
  expect_true(nrow(t_rows) > 0)
  t_val <- t_rows$value[t_rows$statistic_name == "statistic"]
  expect_equal(t_val, -4.062128, tolerance = 1e-4) # t statistic

  # Check lm rows
  lm_rows <- df[df$identifier == "D9_lm", ]
  expect_true(nrow(lm_rows) > 0)

  # Check aov rows
  aov_rows <- df[df$identifier == "npk_aov", ]
  expect_true(nrow(aov_rows) > 0)
  # Find the N:P:K p-value row
  npk_p_row <- aov_rows[!is.na(aov_rows$group_name_2) &
    aov_rows$group_name_2 == "P:K" & aov_rows$statistic_name == "p", ]
  expect_equal(npk_p_row$value, 0.8627521, tolerance = 1e-4)
})
