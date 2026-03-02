# add_stats() -------------------------------------------------------------

test_that("the t-test in main works", {
  result <- tidy_stats(t.test(
    sleep$extra[sleep$group == 1],
    sleep$extra[sleep$group == 2],
    paired = TRUE
  ))

  expect_equal(result$method, "Paired t-test")
  expect_equal(result$statistics[[1]]$value, -1.58,       tolerance = 1e-4) # estimate
  expect_equal(result$statistics[[3]]$value, -4.062128,   tolerance = 1e-4) # t
  expect_equal(result$statistics[[4]]$value, 9,           tolerance = 1e-6) # df
  expect_equal(result$statistics[[5]]$value, 0.00283289,  tolerance = 1e-4) # p
})

test_that("the linear regression in main works", {
  D9 <- data.frame(
    group = gl(2, 10, 20, labels = c("Ctl", "Trt")),
    weight = c(
      4.17, 5.58, 5.18, 6.11, 4.50, 4.61, 5.17, 4.53, 5.33, 5.14, 4.81,
      4.17, 4.41, 3.59, 5.87, 3.83, 6.03, 4.89, 4.32, 4.69
    )
  )

  result <- tidy_stats(lm(weight ~ group, data = D9))

  expect_equal(result$method, "Linear regression")
  expect_equal(result$groups[[1]]$statistics[[1]]$value, 0.0730776,  tolerance = 1e-4) # R2
  expect_equal(result$groups[[1]]$statistics[[6]]$value, 0.2490232,  tolerance = 1e-4) # p

  coefs <- result$groups[[2]]$groups
  expect_equal(coefs[[1]]$statistics[[1]]$value, 5.032,             tolerance = 1e-4) # intercept
  expect_equal(coefs[[2]]$statistics[[1]]$value, -0.371,            tolerance = 1e-4) # groupTrt
  expect_equal(coefs[[2]]$statistics[[5]]$value, 0.2490232,         tolerance = 1e-4) # groupTrt p
})

test_that("the ANOVA in main works", {
  result <- tidy_stats(aov(yield ~ block + N * P * K, npk))

  expect_equal(result$method, "ANOVA")
  terms <- result$groups[[1]]$groups
  expect_equal(terms[[1]]$name, "block")
  expect_equal(terms[[1]]$statistics[[1]]$value, 343.295,   tolerance = 1e-3) # SS
  expect_equal(terms[[1]]$statistics[[6]]$value, 0.01593879, tolerance = 1e-4) # p

  expect_equal(terms[[2]]$name, "N")
  expect_equal(terms[[2]]$statistics[[3]]$value, 12.25873,  tolerance = 1e-4) # F
  expect_equal(terms[[2]]$statistics[[6]]$value, 0.004371812, tolerance = 1e-4) # p
})
