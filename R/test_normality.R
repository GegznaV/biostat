#  test_normality() =========================================================
# TODO:
# 1. Use "parse_formula" for formula interface.
# 2. Enable formaula of form: y1 + y2 + y3 ~ group
# 3. Enable formaula of form: ~ y1 + y2 + y3 | group
#
#

# DONE:
# 1. Order of arguments changed. Now first is `data`
# 2. [+] Prevent from failing if one of the groups do not meet function requirements.
#
# An example of failing:
#
# DATA_1 <- structure(list(
# A = structure(c(2L, 1L, 1L, 1L, 1L, 1L, 1L,
#                 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 1L, 2L, 2L, 2L, 2L, 2L, 2L, 1L,
#                 1L, 2L, 2L, NA, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 1L, 2L, 2L,
#                 2L, 2L, 2L, 2L, 2L, 1L, NA, 2L, 2L, 1L, 1L, 2L, 2L, 1L, 1L, 1L,
#                 1L, 2L, 2L, NA, 2L, 1L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 1L,
#                 1L, 1L, 2L, 2L, 1L, 2L, 2L, NA, 1L, 2L, 2L, 2L, 2L, 2L, 2L, 2L,
#                 2L, 2L, 1L), .Label = c("(-)", "(+)"), class = "factor"),
# B = structure(c(2L, 2L, 2L, 2L, 1L, 1L, 1L, 3L, 1L,
#                 3L, 1L, 1L, 2L, 3L, 3L, 1L, 2L, 2L, 3L, 3L, 3L, 2L, 1L, 2L,
#                 3L, 3L, 3L, 1L, 2L, 1L, 3L, 2L, 2L, 3L, 2L, 2L, 1L, 1L, 1L,
#                 3L, 2L, 1L, 1L, 1L, 3L, NA, 2L, 2L, 3L, 2L, 3L, 1L, 2L, 1L,
#                 1L, 1L, 2L, 2L, NA, 2L, 1L, 1L, 1L, 1L, 2L, NA, 2L, 1L, 2L,
#                 1L, 1L, 1L, 1L, 2L, 1L, 1L, 1L, 1L, 2L, 2L, 1L, 1L, 1L, 1L,
#                 1L, 2L, 1L, 1L, 1L, NA), .Label = c("A", "B",
#                                                     "C"), class = c("ordered", "factor")),
# Y = c(1.41011635102669,
#       2.67996596035883, 2.93082281827539, 3.47057470748745, 3.93013356916371,
#       4.46463889802315, 4.16879679954147, 0.877400732688826, 2.18144836901425,
#       0.43029448325718, 4.62383351283921, 3.81767823167015, 1.12524434294819,
#       0.668998063328768, 0.632398055004286, 5.67433575537348, 2.61454048710063,
#       3.02047582397111, 0.836036933062681, 1.02234637748381, 0.612431298292482,
#       2.41600148898046, 6.25820428291974, 2.72896425226281, 1.04713994687296,
#       1.02055325724726, 0.871851076216876, 2.29227854194425, 2.53371799679094,
#       2.54263453547687, 0.915853083472324, 2.72562238958133, 1.26251201645741,
#       0.639371807195731, 2.21473393518887, 1.05972500710731, 3.0949115598333,
#       3.45374832012055, 4.46001217389848, 1.84533320388126, 2.50946203663001,
#       2.94273769714618, 5.56578156481604, 2.58221811243787, 0.903003765776914,
#       0.61814786719151, 2.01169911590801, 2.48549359503437, 0.707356744235952,
#       1.30002021889422, 0.567361588379272, 5.00332592197709, 5.1262908784939,
#       4.11646207529042, 5.9301390273134, 5.05449658118008, 1.3153282491963,
#       1.62271144298906, 2.99666457392001, 0.975593543621166, 2.4374333607522,
#       2.72788236111502, 5.03425628052577, 3.8940899952299, 1.80900903415956,
#       0.815976697401983, 1.58629992809868, 2.06685279835032, 2.33730373263884,
#       3.51154948522958, 5.5969942369269, 2.05741437514989, 2.22454410505537,
#       1.57830558076523, 3.03264625533027, 7.87293092517559, 3.96414306364804,
#       2.50924620116726, 1.40667280365032, 1.01336405963084, 4.00607224407719,
#       4.1102592110769, 3.71846260372488, 5.73830971585118, 3.86107565341245,
#       3.20207416723376, 4.1743749518175, 2.9932090278708, 4.68758885818583,
#       5.08063093725737)), class = c("tbl_df", "tbl", "data.frame"
#       ), row.names = c(NA, -90L))
#
#
# biostat::test_normality(Y ~ A + B, data = DATA_1)
# # debugonce(biostat:::test_)


# ============================================================================
#' Normality tests by groups
#'
#' Perform a test of normality for each group separately.
#' The available tests: Shapiro-Wilk (default),
#' Lilliefors (Kolmogorov-Smirnov), Anderson-Darling and other.
#'
#' @param y (formula|numeric|character)\cr
#'          Either a formula, a numeric vector or a name of a column
#'             in \code{data}. \itemize{
#'     \item If \code{y} is a formula (e.g. \code{variable ~ factor}), left-hand
#'     side provides the name of a variable to be tested. In the right-hand side
#'     there are names of factor variables to be used to create subsets.
#'     If the left-hand side is empty (e.g. \code{~ factor}), right-hand
#'                                     side is treated as variable name to test.
#' }
#'
#' @param groups (\code{NULL}|factor|character) \cr
#'                An alternative way to provide groups.
#'                If \code{y} is a numeric vector, \code{groups} must be
#'                a factor (or \code{NULL}). If \code{y} is a sting,
#'                \code{groups} must also be a string (or \code{NULL}).
#'
#' @param data (data frame|\code{NULL}) \cr
#'             Either a data frame that contains the
#'             variables mentioned in \code{y} or \code{NULL} (if the variables
#'             are in the function's environment).
#'
#' @param test (string | function) \cr
#'             Either a function that carries out a normality test or
#'             a string (case insensitive, maybe unambiguously abbreviated)
#'              with a name of a normality test. Possible names of tests:
#'             \itemize{
#'     \item{"SW", "Shapiro-Wilk" — for Shapiro-Wilk test;}
#'     \item{"Lilliefors" — for Kolmogorov-Smirnov test with Lilliefor's correction;}
#'     \item{"AD", "Anderson-Darling" — for Anderson-Darling test;}
#'     \item{"CVM", "CM", "Cramer-von Mises" — for Cramer-von Mises test;}
#'     \item{"SF", "Shapiro-Francia" — for Shapiro-Francia test;}
#'     \item{"Chi-squared","Pearsons" — for Pearson's chi-squared test of normality.}
#' }
#'
#' @param signif_stars (logical) \cr
#'                     If \code{TRUE}, significance stars are printed.
#'
#' @param legend (logical) \cr
#'                     If \code{TRUE}, legend for significance stars
#'                     is printed.
#'
#' @param digits_stat (integer)  \cr
#'                     Either a number of decimal places or number of
#'                     significant digits to round test statistic to.
#'
#' @param format_stat (character) \cr
#'                     Number format "f", "g" or "auto" (default) for test
#'                     statistic. More about number
#'                    formats "f" and "g" you can find in documentation of
#'                    function \code{\link[base]{formatC}} section \code{format}.
#'
#' @param show_col_method (logical) \cr
#'                     If \code{FALSE} column "method" is not
#'                   printed. Default is \code{FALSE}.
#' @param hide_error_msg (logical) \cr
#'                     If \code{FALSE} column "error_msg" is not printed.
#'                     If \code{TRUE} the column is printed if error occurs
#'                     in calcultations.
#'                     Default is \code{FALSE}.
#'
#' @param caption (string|\code{NULL}|\code{NA}) \cr
#'                     A caption for the table with
#'                results of a normality test. If \code{NA} — a default caption
#'                is printed (default). If \code{NULL} – no caption is printed.
#'
#' @param p_adjust_method (\code{NULL}|string) \cr
#'                     A name of p value adjustment
#'               method for multiple comparisons. For available methods check
#'               \code{\link[stats]{p.adjust.methods}}.
#'               If \code{NULL}, no adjusted p value is calculated (default).
#'               If string (e.g., \code{"holm"}), an additional column
#'               \code{p.adjust} is calculated.
#'
#' @param ... Further parameters to the function of normatity test.
#'
#' @param x \code{normality_test} object.
#'
#' @inheritParams format_p_values
#' @inheritParams stats::shapiro.test
#' @inheritParams nortest::lillie.test
#' @inheritParams nortest::pearson.test
#'
#'
#' @return  \itemize{
#'       \item Function \code{test_normality} returns a data frame with
#'             normality test results for each group.
#'       \item \code{print} and \code{pander} methods format and print the
#'       results. By default, methods \code{print.test_normality} and
#'       \code{pander.test_normality} do not print column called "method".
#' }
#'
#'
#' @export
#'
#' @importFrom stats shapiro.test
#' @import nortest
#'
#' @seealso \itemize{
#'   \item \code{\link[stats]{shapiro.test}} in package \pkg{stats};
#'   \item \code{\link[nortest]{lillie.test}} in package \pkg{nortest};
#'   \item \code{\link[nortest]{pearson.test}} in package \pkg{nortest};
#'   \item \code{\link[nortest]{ad.test}} in package \pkg{nortest};
#'   \item \code{\link[nortest]{cvm.test}} in package \pkg{nortest};
#'   \item \code{\link[nortest]{sf.test}} in package \pkg{nortest}.
#' }
#'
#' @examples
#' library(biostat)
#' library(pander)
#' data(CO2)
#'
#' # For whole dataset
#' test_normality(~uptake, data = CO2)
#'
#' # For each subgroup
#' test_normality(uptake ~ Treatment, data = CO2)
#'
#' # For each subgroup by several factor variables
#' rez <- test_normality(uptake ~ Type + Treatment,
#'                       data = CO2)
#' rez
#'
#' # Modify printed output
#' print(rez, rm_zero = TRUE)
#'
#' print(rez, digits_p = 2)
#'
#'
#' # Choose another test of normality
#' rez2 <- test_normality(uptake ~ Type + Treatment,
#'                       data = CO2,
#'                       test = "chi-sq")
#' rez2
#'
#'
#' # Print as a 'pandoc' table (for R Markdown reports)
#' pander(rez)
#'
#' pander(rez, digits_stat = 2)
#'
#' pander(rez, digits_p = 2)
#'
#' pander(rez, digits_p = 4, signif_stars = FALSE)
#'
#'
#' \donttest{\dontrun{
#' # View unformatted results in a separate window
#' View(rez)
#' }}
#'
#' # Show object's class
#' class(rez)
#'
test_normality <- function(y,
                           data = NULL,
                           test = "Shapiro-Wilk",
                           p_adjust_method = NULL,
                           ...,
                           groups = NULL,
                           ss = signif_syms,
                           hide_error_msg = FALSE) {

  # Choose the test ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (is.function(test)) {
    use_test <- test
  } else if (checkmate::test_character(test, len = 1)) {
    # Possible choices
    available_tests <- c(
      "SW", "Shapiro-Wilk",
      "Lilliefors",
      "AD", "Anderson-Darling",
      "CVM", "CM", "Cramer-von Mises",
      "SF", "Shapiro-Francia",
      "Chi-squared", "Pearsons"
    )

    test <- match.arg(tolower(test), tolower(available_tests))

    use_test <- switch(
      test,
      "sw" = ,
      "shapiro.test" = ,
      "shapiro-wilk" = stats::shapiro.test,

      "lillie.test" = ,
      "lilliefors" = nortest::lillie.test,

      "ad" = ,
      "ad.test" = ,
      "anderson-darling" = nortest::ad.test,

      "cvm" = ,
      "cm" = ,
      "cvm.test" = ,
      "cramer-von mises" = nortest::cvm.test,

      "sf" = ,
      "sf.test" = ,
      "shapiro-francia" = nortest::sf.test,

      "chi-squared" = ,
      "pearson.test" = ,
      "pearsons" = nortest::pearson.test
    )
  } else {
    stop(
      "\n`test` must be either a function",
      "\n or a name of a test (a string of length 1)."
    )
  }

  use_test_mod <- function(...) {
    safe_rez <- purrr::safely(use_test, otherwise = NULL)(...)

    if (is.null(safe_rez$error)) {
      out <- safe_rez$result
      out$error_msg <- NA_character_
    } else {
      out <- structure(
        data.frame(
          method = as.character(safe_rez$error),
          stringsAsFactors = FALSE
        ),
        class = "htest"
      )
    }
    out
  }

  # Output
  test_(y,
    data = data,
    p_adjust_method = p_adjust_method,
    ...,
    groups = groups,
    test = use_test_mod,
    hide_error_msg = hide_error_msg,
    ss = ss
  )
}


# test_() =====================================================================
test_ <- function(y,
                  data = NULL,
                  p_adjust_method = NULL,
                  ...,
                  groups = NULL,
                  test = stats::shapiro.test,
                  ss = NULL,
                  hide_error_msg = FALSE)
# na.rm = getOption("na.rm", FALSE)
{
  # Make formula according to input type
  if (is.numeric(y)) {
    if (!is.null(groups)) {
      data <- data.frame(y = y, groups = groups)
      y <- y ~ groups
    } else {
      data <- data.frame(y = y)
      y <- ~y
    }
  }

  if (is.null(data)) {
    data <- rlang::f_env(y)
  }

  if (rlang::is_formula(y)) {
    # Select necessary variables only
    data <- stats::model.frame(y, data = data)

    # To indicate, that there is no grouping the first column constant
    # if (ncol(data) == 1)
    #     data[["Groups"]] <- "<all values>"

    var_names <- names(data)
    gr_vars <- rlang::syms(var_names[-1])

    if (!is.numeric(data[[1]])) {
      stop_glue("Variable `{var_names[1]}` must be numeric.")
    }

    # The main test
    rez <-
      data %>%
      dplyr::group_by(!!!gr_vars) %>%
      dplyr::do(broom::tidy(test(.[[1]], ...))) %>%
      dplyr::ungroup() %>%
      dplyr::select(method, dplyr::everything()) %>%
      as.data.frame()

    # Add error message corrently
    err <- stringr::str_detect(rez$method, "([Ee]rror)|([Ww]arning)")
    rez$error_msg[err] <- rez$method[err]
    rez$method[err] <- unique(rez$method[!err])


    # If adjusted p value is needed
    if (!is.null(p_adjust_method)) {
      rez$p.adjust <- p.adjust(rez$p.value, method = p_adjust_method)
    }

    rez <- structure(rez,
      class = c("test_normality", "data.frame"),
      test = as.character(unique(rez$method)),
      p_adjust_method = p_adjust_method,
      hide_error_msg = hide_error_msg,
      ss = ss
    )

    return(rez)
  } else {
    stop("Incorrect input")
  }
}


# print()  ----------------------------------------------------------------
#' @rdname test_normality
#' @export
print.test_normality <- function(x,
                                 ...,
                                 digits_p = 3,
                                 signif_stars = !is.null(ss),
                                 digits_stat = 3,
                                 format_stat = c("auto", "f", "g"),
                                 rm_zero = FALSE,
                                 legend = TRUE,
                                 show_col_method = FALSE,
                                 hide_error_msg = attr(x, "hide_error_msg"),
                                 ss = attr(x, "ss")) {
  format_stat <- match.arg(format_stat)
  x <- format_object(
    x,
    digits_p = digits_p,
    digits_stat = digits_stat,
    format_stat = format_stat,
    signif_stars = signif_stars,
    signif_as_separate_column = TRUE,
    signif_stars_col_name = " ",
    rm_zero = rm_zero,
    show_col_method = show_col_method,
    ss = ss,
    hide_error_msg = isTRUE(hide_error_msg)
  )



  # Print the name of the method
  cat("\n", "The results of", which_test(x), "\n\n")

  # Print main results
  NextMethod(print, x)

  # Print signif. stars legend
  if (legend == TRUE && signif_stars == TRUE) {
    cat(paste0(
      "\n",
      # "Legend for p-values:  \n",
      signif_stars_legend(ss = ss, collapse = ", "), "\n"
    ))
  }

  # Print p adjust. method:
  p_adjust_method <- attr(x, "p_adjust_method")
  if (!is.null(p_adjust_method) && nrow(x) > 1) {
    cat(
      "The method for p-value adjustment:",
      first_capital(p_adjust_method),
      "\n"
    )
  }
}

# pander()  ----------------------------------------------------------------
#' @export
#' @rdname test_normality
pander.test_normality <- function(
                                  x,
                                  caption = NA,
                                  ...,
                                  digits_p = 3,
                                  signif_stars = !is.null(ss),
                                  digits_stat = 3,
                                  format_stat = c("auto", "f", "g"),
                                  rm_zero = FALSE,
                                  legend = TRUE,
                                  show_col_method = FALSE,
                                  hide_error_msg = attr(x, "hide_error_msg"),
                                  ss = attr(x, "ss")) {
  format_stat <- match.arg(format_stat)

  x <- format_object(
    x,
    digits_p = digits_p,
    digits_stat = digits_stat,
    format_stat = format_stat,
    signif_stars = signif_stars,
    signif_as_separate_column = FALSE,
    rm_zero = rm_zero,
    show_col_method = show_col_method,
    ss = ss,
    hide_error_msg = hide_error_msg
  )

  # String for p adjust. method:
  p_adjust_method <- attr(x, "p_adjust_method")
  adj_sting <-
    if (!is.null(p_adjust_method) & nrow(x) > 1) {
      paste0(
        " The method for p-value adjustment: ",
        first_capital(p_adjust_method), "."
      )
    } else {
      ""
    }

  # Add caption
  caption <-
    if (is.null(caption)) {
      NULL
    } else if (is.na(caption)) {
      glue::glue("The results of {which_test(x)}.{adj_sting}")
    } else {
      caption
    }

  # Print table of results
  NextMethod("pander", x, caption = caption, ...)

  # Print legend
  if (legend == TRUE && signif_stars == TRUE) {
    # cat("Legend for p-values:  \n`", signif_stars_legend(ss = ss), "`  \n")
    cat(paste0(
      # "\nLegend for p-values:  \n",
      signif_stars_legend(ss = ss), "\n"
    ))
  }
}

# helpers --------------------------------------------------------------------
which_test <- function(x) {
  # as.character(unique(x$method))
  attr(x, "test")
}
