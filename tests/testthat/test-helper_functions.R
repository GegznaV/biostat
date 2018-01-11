context("helper_functions")

test_that("first_capital works", {
    expect_equal(BioStat:::first_capital("aaa bbb ccc;ddd-eee"), "Aaa Bbb Ccc;Ddd-Eee")
})

test_that("eval_ works", {
    a <- 1
    expect_equal(BioStat:::eval_("a"), 1)
})

test_that("head_tail works", {
    a <- 1
    expect_length(BioStat:::head_tail(chickwts), 2)
    expect_equal(nrow(BioStat:::head_tail(chickwts)), 10)
    expect_output(print(BioStat:::head_tail(chickwts)),
                  "\\.\\.\\.    \\.\\.\\.       \\.\\.\\.")
})

test_that("warning_glue works", {
    a <- 123
    expect_warning(BioStat:::warning_glue("{a}"))
})

test_that("stop_glue works", {
    a <- 123
    expect_message(BioStat:::message_glue("{a}"))
})

test_that("stop_glue works", {
    a <- 123
    expect_error(BioStat:::stop_glue("{a}"))
})


test_that("class_stat_summary works", {
    expect_is(BioStat:::class_stat_summary(list("a")), "stat_summary")
})


test_that("scale_vector works", {

    x <- 1:10
    expect_equal(mean(scale_vector(x)), 0)
    expect_equal(sd(scale_vector(x)), 1)

    expect_equal(mean(scale_vector(x, center = median)), 0)
    expect_equal(IQR(scale_vector(x, scale = IQR)), 1)

    expect_error(median(scale_vector(x, center = 1:2)))
    expect_error(IQR(scale_vector(x, scale = 1:2)))

})