# Setup -------------------------------------------------------------------

statistics <- list()

# t.test(), lm(), and aov() -----------------------------------------------

D9 <- tibble::tibble(
  group = gl(2, 10, 20, labels = c("Ctl", "Trt")),
  weight = c(
    4.17, 5.58, 5.18, 6.11, 4.50, 4.61, 5.17, 4.53, 5.33, 5.14, 4.81, 4.17,
    4.41, 3.59, 5.87, 3.83, 6.03, 4.89, 4.32, 4.69
  )
)

sleep_t_test <- t.test(extra ~ group, data = sleep, paired = TRUE)
D9_lm <- lm(weight ~ group, data = D9)
npk_aov <- aov(yield ~ block + N * P * K, npk)

statistics <- statistics |>
  add_stats(sleep_t_test, type = "primary") |>
  add_stats(D9_lm, preregistered = FALSE) |>
  add_stats(npk_aov, notes = "An ANOVA example")

sleep_t_test
summary(D9_lm)
summary(npk_aov)

# add_stats(): default identifier -----------------------------------------

# Statistical test with piping
t.test(quote_source$response, alternative = "greater") |>
  add_stats(statistics, .)

# tidy_stats_to_data_frame() ----------------------------------------------

df <- tidy_stats_to_data_frame(statistics)
write_csv(df, "tests/data/main_df.csv")

# write_stats() -----------------------------------------------------------

write_test_stats(statistics, "tests/data/main.json")

# Cleanup -----------------------------------------------------------------

rm(sleep_t_test, D9, D9_lm, npk_aov, df, statistics)
