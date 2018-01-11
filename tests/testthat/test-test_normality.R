context("Function `test_normality()`")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
test_that("`test_normality()` produces object with correct classes", {
    data(CO2, package = "datasets")
    rez <- test_normality(uptake ~ Type, data = CO2)

    expect_is(rez, c("data.frame"))
    expect_is(rez, c("test_normality"))
})

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
test_that("`test_normality()` produces correct attributes", {
    data(CO2, package = "datasets")
    rez <- test_normality(uptake ~ Type, data = CO2, p_adjust_method = "holm")

    expect_equal(attr(rez, "test"), "Shapiro-Wilk normality test")
    expect_equal(attr(rez, "p_adjust_method"), "holm")
})
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
test_that("`test_normality()` works with one-sided formula `~var`", {
    data(CO2, package = "datasets")
    rez <- test_normality( ~ uptake, data = CO2)

    expect_is(rez, c("data.frame"))
    expect_is(rez, c("test_normality"))
    expect_equal(nrow(rez), 1)
})
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
test_that("`test_normality()` produces object with correct classes", {
    data(CO2, package = "datasets")
    rez <- test_normality(uptake ~ Type, data = CO2)

    expect_is(rez, c("data.frame"))
    expect_is(rez, c("test_normality"))
})

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
test_that("parameter test = 'chi-squared' for `test_normality()` is recognized correctly", {
    data(CO2, package = "datasets")

    rez1 <- test_normality(uptake ~ Type, data = CO2, test = "chi-squared")
    rez2 <- test_normality(uptake ~ Type, data = CO2, test = "chi")
    rez3 <- test_normality(uptake ~ Type, data = CO2, test = "Pearson")
    rez4 <- test_normality(uptake ~ Type, data = CO2, test = "pearson")

    expect_true(unique(rez1$method) == "Pearson chi-square normality test")
    expect_true(unique(rez2$method) == "Pearson chi-square normality test")
    expect_true(unique(rez3$method) == "Pearson chi-square normality test")
    expect_true(unique(rez4$method) == "Pearson chi-square normality test")

})
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
test_that("parameter test = 'Shapiro-Wilk' for `test_normality()` is recognized correctly", {
    data(CO2, package = "datasets")

    rez1 <- test_normality(uptake ~ Type, data = CO2)
    rez2 <- test_normality(uptake ~ Type, data = CO2, test = "Shapiro-Wilk")
    rez3 <- test_normality(uptake ~ Type, data = CO2, test = "sW")
    rez4 <- test_normality(uptake ~ Type, data = CO2, test = "SW")

    expect_true(unique(rez1$method) == "Shapiro-Wilk normality test")
    expect_true(unique(rez2$method) == "Shapiro-Wilk normality test")
    expect_true(unique(rez3$method) == "Shapiro-Wilk normality test")
    expect_true(unique(rez4$method) == "Shapiro-Wilk normality test")

})
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
test_that("parameter test = 'lilliefors' for `test_normality()` is recognized correctly", {
    data(CO2, package = "datasets")

    rez1 <- test_normality(uptake ~ Type, data = CO2, test = "lillie")
    rez2 <- test_normality(uptake ~ Type, data = CO2, test = "Lilliefors")

    expect_true(unique(rez1$method) == "Lilliefors (Kolmogorov-Smirnov) normality test")
    expect_true(unique(rez2$method) == "Lilliefors (Kolmogorov-Smirnov) normality test")

})
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
test_that("`test_normality()` produces correct p-values for each group", {
    data(CO2, package = "datasets")

    rez <- test_normality(uptake ~ Type, data = CO2)
    filter <- dplyr::filter

    expect_equal(             filter(rez, Type == "Quebec")        $p.value,
                 shapiro.test(filter(CO2, Type == "Quebec")$uptake)$p.value)

    expect_equal(             filter(rez, Type == "Mississippi")        $p.value,
                 shapiro.test(filter(CO2, Type == "Mississippi")$uptake)$p.value)

})
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
test_that("`print()` method for 'test_normality' object works", {
    data(CO2, package = "datasets")

    rez <- test_normality(uptake ~ Type, data = CO2)

    expect_output(print(rez), "normality test")
    expect_output(print(rez), "statistic")
    expect_output(print(rez), "p.value")
})

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
test_that("`pander()` method for 'test_normality' object works", {
    data(CO2, package = "datasets")

    rez <- test_normality(uptake ~ Type, data = CO2)


    # Expect specific structure
    expect_output(pander(rez), "statistic")
    expect_output(pander(rez), "p.value")
    expect_output(pander(rez), "-------------")
    expect_output(pander(rez), "Table: ")
    expect_output(pander(rez), "The results of")
    expect_output(pander(rez), "normality test")

    # Expect specific caption
    expect_output(pander(rez, caption = "LA LA LA"), "Table: LA LA LA")

    # Output must not contain "Table: ", when `caption = NULL`
    output <- capture.output(pander(rez, caption = NULL))
    expect_true(all(!(grepl("Table: ", output))))
})
