context("ci_mean_t")

test_that("ci_mean_t works", {
  set.seed(999555)
  x <- rnorm(35, 10, 5)
  rez <- ci_mean_t(x)

  expect_is(ci_mean_t(x, return_df = TRUE), "data.frame")
  expect_is(ci_mean_t(x, return_df = FALSE), "matrix")

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  expect_equivalent(
    ci_mean_t(c(1:30, NA, NA, NA), na.rm = FALSE),
    c(NA, NA, NA, 0.95)
  )

  expect_equivalent(
    ci_mean_t(c(1:30, NA, NA, NA), na.rm = TRUE),
    ci_mean_t(c(1:30))
  )
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
})
