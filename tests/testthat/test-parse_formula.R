context("test-parse_formula.R")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
test_that("`parse_formula` parse factors correctly", {
  obj <- biostat:::parse_formula(decrease ~ treatment, OrchardSprays)

  expect_identical(obj$data$decrease, OrchardSprays$decrease)
  expect_identical(obj$data$treatment, OrchardSprays$treatment)
})

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
test_that("parse two-sided formula correctly", {
  obj <- biostat:::parse_formula(
    log(decrease) ~ decrease + treatment + log(rowpos) | colpos,
    data = OrchardSprays
  )

  expect_length(OrchardSprays, 4)
  expect_length(obj$data, 5)

  expect_equal(
    names(obj$data),
    c(
      "log(decrease)",
      "decrease",
      "treatment",
      "log(rowpos)",
      "colpos"
    )
  )

  expect_equal(
    obj$names$formula,
    c(
      "log(decrease)",
      "decrease",
      "treatment",
      "log(rowpos)",
      "colpos"
    )
  )
  expect_equal(obj$names$y, c("log(decrease)"))
  expect_equal(obj$names$lhs, c("log(decrease)"))

  expect_equal(obj$names$x, c("decrease", "treatment", "log(rowpos)"))
  expect_equal(obj$names$rhs, c("decrease", "treatment", "log(rowpos)"))

  expect_equal(obj$names$condition, c("colpos"))
})
