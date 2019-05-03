context("ci_mean_boot")

test_that("ci_mean_boot works", {
    set.seed(999555)
    x <- rnorm(35, 10, 5)

    expect_is(ci_mean_boot(x, return_df = TRUE),  "data.frame")
    expect_is(ci_mean_boot(x, return_df = FALSE), "matrix")

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    set.seed(999555)
    expect_error(ci_mean_boot(c(1:30, NA, NA, NA), na.rm = FALSE))

    set.seed(999555)
    expect_equivalent(
        round(ci_mean_boot(c(1:30, NA, NA, NA), na.rm = TRUE), 2),
        c(15.5, 12.4, 18.6, 0.95, 2000))
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    set.seed(999555)

    get_means <- function(x) {
        attributes(x)$resampled_means
    }

    res_a <- get_means(ci_mean_boot(c(1:30)))
    expect_null(res_a)

    res_b <- get_means(ci_mean_boot(c(1:30), resampled_means = TRUE, repetitions = 2000))
    expect_is(res_b, "numeric")
    expect_length(res_b, 2000)

    res_c <- get_means(ci_mean_boot(c(1:30), resampled_means = TRUE, repetitions = 200))
    expect_length(res_c, 200)
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

})
