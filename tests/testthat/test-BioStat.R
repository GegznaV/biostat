
# context("Operator `%++%`")
#
# test_that("`%++%` works", {
#     expect_equal("a" %++% "b", "ab")
# })

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
context("Function `qq_line_coeffs`")

test_that("`qq_line_coeffs()` works", {
    set.seed(254)
    rez <- qq_line_coeffs(rnorm(50))
    expect_true(is.vector(rez))
    expect_equal(names(rez), c("intercept", "slope"))

    # Approximate rezult
    expect_equal(round(rez, 7),
                 c(intercept = 0.1078016, slope = 0.8185576))
})

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
context("Function `SIGNIF`")

test_that("`SIGNIF()` works", {
    expect_equal(SIGNIF(0.005),    0.005)
    expect_is(SIGNIF(0.005),   "numeric")
    expect_is(SIGNIF("0.005"), "numeric")
})
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
context("Function `adjust_vector_length`")

test_that("`adjust_vector_length()` works", {
    expect_length(adjust_vector_length(2, CO2),    length(CO2))
    expect_length(adjust_vector_length(1:5, CO2),    length(CO2))
    expect_error(adjust_vector_length(2:3, CO2))

})


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
context("Function `format_numbers()`")

test_that("`format_numbers()` works", {
    DATA <- head(iris)


    # The same rounding for each column

    df1 <- format_numbers(DATA, 1)

    expect_equal(df1[[1]][1], "5.1")
    expect_equal(df1[[2]][1], "3.5")
    expect_equal(df1[[3]][1], "1.4")
    expect_equal(df1[[4]][1], "0.2")


    df2 <- format_numbers(DATA, 2)

    expect_equal(df2[[1]][1], "5.10")
    expect_equal(df2[[2]][1], "3.50")
    expect_equal(df2[[3]][1], "1.40")
    expect_equal(df2[[4]][1], "0.20")
    # Different rounding for different columns

    df3 <- format_numbers(DATA, c(1,2,3,3,NA))

    expect_equal(df3[[1]][1], "5.1")
    expect_equal(df3[[2]][1], "3.50")
    expect_equal(df3[[3]][1], "1.400")
    expect_equal(df3[[4]][1], "0.200")
})
