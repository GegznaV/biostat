context("coef_standardized")

test_that("`coef_standardized()` works", {
  data(USJudgeRatings)
  lm1 <- lm(CONT ~ INTG + DMNR + log(DILG), data = USJudgeRatings)

  rez <- coef_standardized(lm1)

  expect_equal(rez$beta, QuantPsyc::lm.beta(lm1))
  expect_equivalent(unclass(round(rez$beta, 3)), c(-0.293, -0.316, 0.533))
})

test_that("`coef_standardized()` works if intercept is not included", {
  data(USJudgeRatings)
  lm2 <- lm(CONT ~ 0 + INTG + DMNR + log(DILG), data = USJudgeRatings)

  rez <- coef_standardized(lm2)

  expect_equivalent(unclass(round(rez$beta, 3)), c(-0.012, -0.747, 0.795))
})

test_that("`print.lm_beta()` works", {
  data(USJudgeRatings)
  lm1 <- lm(CONT ~ INTG + DMNR + log(DILG), data = USJudgeRatings)
  rez <- coef_standardized(lm1)

  expect_is(rez, "lm_beta")
  expect_is(rez, "list")

  expect_output(print(rez), "Standardized Regression Coefficients:")
  expect_output(print(rez), "-0.293")
  expect_output(print(rez), "-0.316")
  expect_output(print(rez), "0.533")
  expect_output(print(rez), "log\\(DILG\\)")
})

test_that("`print.lm_beta()` works", {
  data(USJudgeRatings)
  lm1 <- lm(CONT ~ INTG + DMNR + log(DILG), data = USJudgeRatings)
  rez <- coef_standardized(lm1)

  rez_summ <- summary(rez)

  expect_is(rez_summ, "lm_beta_summary")
  expect_is(rez_summ, "data.frame")

  expect_true(is.na(rez_summ$influence_rank[1]))
  expect_true(is.na(rez_summ$standardized_coeff[1]))
  expect_equal(nrow(rez_summ), 4)
  expect_equivalent(rez_summ$coeff, coef(lm1))
})
