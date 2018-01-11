# library(testthat)

context("Function `trim()`")

test_that("`trim()` works", {

    expect_is(trim(1:10), "integer")
    expect_error(trim(letters))
    expect_error(trim(as.data.frame(letters)))

    expect_length(trim(1:100, trim = 0.1), 90)
    expect_length(trim(1:100, trim = 0.2), 80)
    expect_length(trim(1:100, trim = 0.5), 50)

})



