context("format_p_values")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
context("Function `format_p_values`")

test_that("`format_p_values()` works with numbers", {
  expect_equal(format_p_values(0.000005, digits_p = 5, signif_stars = FALSE), "<0.00001")
  expect_equal(format_p_values(0.005, digits_p = 3, signif_stars = FALSE), " 0.005")
  expect_equal(format_p_values(0.005, digits_p = 2, signif_stars = FALSE), "<0.01")
  expect_equal(format_p_values(0.0005, digits_p = 2, signif_stars = FALSE), "<0.001")
  expect_equal(format_p_values(0.052147, digits_p = 2, signif_stars = FALSE), " 0.05")
})
test_that("`prettify_p_value()` works with characters", {
  expect_equal(format_p_values("0.005", digits_p = 2, signif_stars = FALSE), "<0.01")
  expect_equal(format_p_values("0.0005", digits_p = 2, signif_stars = FALSE), "<0.001")
  expect_equal(format_p_values("0.052147", digits_p = 2, signif_stars = FALSE), " 0.05")
})
test_that("`prettify_p_value()` works with factors", {
  expect_equal(format_p_values(factor("0.005"), digits_p = 2, signif_stars = FALSE), "<0.01")
  expect_equal(format_p_values(factor("0.0005"), digits_p = 2, signif_stars = FALSE), "<0.001")
  expect_equal(format_p_values(factor("0.052147"), digits_p = 2, signif_stars = FALSE), " 0.05")
})

test_that("`format_p_values(signif_stars = TRUE)`  works", {
  expect_equal(format_p_values(0.005, digits_p = 2, signif_stars = TRUE), "<0.01 ** ")
  expect_equal(format_p_values(0.0005, digits_p = 2, signif_stars = TRUE), "<0.001 ***")
  expect_equal(format_p_values(0.052147, digits_p = 2, signif_stars = TRUE), " 0.05 .  ")
})
test_that("`format_p_values(rm_zero = TRUE)`  works", {
  expect_equal(format_p_values(0.052147, digits_p = 2, signif_stars = FALSE, rm_zero = TRUE), " .05")
})
test_that("`format_p_values(digits_p = 3)` works with numbers", {
  expect_equal(format_p_values(0.005, digits_p = 3, signif_stars = FALSE), " 0.005")
  expect_equal(format_p_values(0.0005, digits_p = 3, signif_stars = FALSE), "<0.001")
  expect_equal(format_p_values(0.052147, digits_p = 3, signif_stars = FALSE), " 0.052")
})

test_that("input range of `prettify_p_value()`", {
  # Input range is between 0 and 1
  expect_error(format_p_values(-2))
  expect_error(format_p_values(2))
})

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
context("Function `format_as_p_columns`")

test_that("`format_as_p_columns()` changes column 'p.value'", {
  data("CO2")
  data <- test_normality(uptake ~ Type, data = CO2)
  expect_warning(rez <- format_as_p_columns(data))

  classes_before <- purrr::map_chr(data, ~ class(.))
  classes_after <- purrr::map_chr(rez, ~ class(.))

  # Classes of other columns than "p.value" must not change
  expect_true(all(classes_before[-c(4)] == classes_after[-c(4)]))

  # Class of colmn "p.value" changes to "character"
  expect_match(classes_before["p.value"], "numeric")
  expect_match(classes_after["p.value"], "character")
})

test_that("`format_as_p_columns()` changes column 'p.adjust'", {
  data("CO2")
  data <- test_normality(uptake ~ Type, data = CO2, p_adjust_method = "holm")
  expect_warning(rez <- format_as_p_columns(data))

  classes_before <- purrr::map_chr(data, ~ class(.))
  classes_after <- purrr::map_chr(rez, ~ class(.))

  # Classes of other columns than "p.value" must not change
  expect_true(all(classes_before[-c(4, 5)] == classes_after[-c(4, 5)]))

  # Class of colmn ""p.adjust" changes to "character"
  expect_match(classes_before["p.adjust"], "numeric")
  expect_match(classes_after["p.adjust"], "character")
})




# ============================================================================
#
# # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# context("Function `prettify_p_value`")
#
# test_that("`prettify_p_value()` works with numbers", {
#     expect_equal(prettify_p_value(0.005),    "<0.01 ")
#     expect_equal(prettify_p_value(0.0005),   "<0.001")
#     expect_equal(prettify_p_value(0.052147), " 0.05 ")
# })
# test_that("`prettify_p_value()` works with characters", {
#     expect_equal(prettify_p_value("0.005"),    "<0.01 ")
#     expect_equal(prettify_p_value("0.0005"),   "<0.001")
#     expect_equal(prettify_p_value("0.052147"), " 0.05 ")
# })
# test_that("`prettify_p_value()` works with factors", {
#     expect_equal(prettify_p_value(factor("0.005")),    "<0.01 ")
#     expect_equal(prettify_p_value(factor("0.0005")),   "<0.001")
#     expect_equal(prettify_p_value(factor("0.052147")), " 0.05 ")
# })
# test_that("input range of `prettify_p_value()`", {
#     # Input range is between 0 and 1
#     expect_error(prettify_p_value(-2))
#     expect_error(prettify_p_value(2))
#
#     # No more than one input value is accepted
#     expect_error(prettify_p_value(c(0.5,0.6)))
#
# })
#
# # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# context("Function `prettify_p_column`")
#
# test_that("`prettify_p_column()` changes column 'p.value'", {
#
#     data("CO2")
#     data <- test_normality(uptake ~ Type, data = CO2)
#     rez  <- prettify_p_column(data)
#
#     classes_before <- purrr::map_chr(data, ~class(.))
#     classes_after  <- purrr::map_chr(rez,  ~class(.))
#
#     # Classes of other columns than "p.value" must not change
#     expect_true(all(classes_before[-c(3, 5)] ==  classes_after[-c(3, 5)]))
#
#     # Class of colmn "p.value" changes to "character"
#     expect_match(classes_before["p.value"], "numeric")
#     expect_match(classes_after["p.value"], "character")
#
# })
#
# test_that("`prettify_p_column()` changes column 'p.adjust'", {
#
#     data("CO2")
#     data <- test_normality(uptake ~ Type, data = CO2)
#     rez  <- prettify_p_column(data)
#
#     classes_before <- purrr::map_chr(data, ~class(.))
#     classes_after  <- purrr::map_chr(rez,  ~class(.))
#
#     # Classes of other columns than "p.value" must not change
#     expect_true(all(classes_before[-c(3, 5)] ==  classes_after[-c(3, 5)]))
#
#     # Class of colmn ""p.adjust" changes to "character"
#     expect_match(classes_before["p.adjust"], "numeric")
#     expect_match(classes_after["p.adjust"], "character")
# })
