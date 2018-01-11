## Unit teststest_that("format_pvalue works", {
# expect_equal(biostat:::format_pvalue(.3), "p = 0.3")
# })
#
# test_that("noZero works", {
#     expect_equal(biostat:::noZero(0.3), ".3")
# })


# Function to format p values nicely
# from package `userfriendlyscience`

format_pvalue <- function(values,
                          digits = 3,
                          spaces = TRUE,
                          includeP = TRUE) {
    missingValues <- is.na(values)

        values <- ifelse(values < 0,
                         0,
                         ifelse(values > 1, 1, values))

        pchar <- ifelse(includeP, "p = ", "")
        eps <- 10 ^ -digits
        res <- paste0(pchar,
                      # noZero(
                          format.pval(
                              round(values, digits),
                              eps = eps,
                              digits = digits,
                              scientific = digits + 1
                          )
                      # )
        )

        if (spaces) {
            res <- gsub("= <", "< ", res)

        } else {
            res <- gsub("= <", "<", res)

            res <- gsub(" ", "", res)

        }
        res <- ifelse(missingValues, NA, res)

        return(res)

    }

### Function to remove zero at start of number
noZero <- function(str) {
    return(gsub("0\\.", ".", str))

}
