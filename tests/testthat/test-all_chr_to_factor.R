# library(testthat)

context("Function `all_chr_to_factor()` ")

test_that("`all_chr_to_factor()` works with data frames", {
    # Create a data frame
    df <- data.frame(letters  = letters[1:5],
                     letters2 = LETTERS[1:5],
                     numbers  = 1:5,
                     stringsAsFactors = FALSE)


    expect_is(df[[1]], "character")
    expect_is(df[[2]], "character")
    expect_is(df[[3]], "integer")

    # Convert all character variables to strings
    df2 <- all_chr_to_factor(df)

    expect_is(df2[[1]], "factor")
    expect_is(df2[[2]], "factor")
    expect_is(df2[[3]], "integer")

})

test_that("`all_chr_to_factor()` works with tibbles", {
    # Create a data frame
    df <- tibble::tibble(letters  = letters[1:5],
                     letters2 = LETTERS[1:5],
                     numbers  = 1:5,
                     stringsAsFactors = FALSE)


    expect_is(df[[1]], "character")
    expect_is(df[[2]], "character")
    expect_is(df[[3]], "integer")

    # Convert all character variables to strings
    df2 <- all_chr_to_factor(df)

    expect_is(df2[[1]], "factor")
    expect_is(df2[[2]], "factor")
    expect_is(df2[[3]], "integer")

})

