# TODO
# 1. possibly change function name
# 2. code review is needed.

#' Do numerical summaries by groups
#'
#' Do numerical summaries by groups with formaula interface. Missing values are automatically removed.
#'
#' @param y formula with variable names to summarize. See more in examples.
#' @param data data set
#' @param stat (character) Descriptive statistics to compute. Currently supported statistics:
#'  \itemize{
#'      \item `"n"` - number of non-missing observations,
#'      \item `"missing"` - number of missing observations,,
#'      \item `"mean"` - arithmetic mean,
#'      \item `"sd"` - standard deviation,
#'      \item `"variance"` - variance,
#'      \item `"trimmed"` - trimmed mean,
#'      \item `"min"` - minimum value,
#'      \item `"Q1"` - 1-st quartile,
#'      \item `"Md"` - median,
#'      \item `"Q3"` - 3-rd quartile,
#'      \item `"max"` - maximum value,
#'      \item `"mad"` - median absolute deviation from median (more details [mad][stats::mad]),
#'      \item `"IQR"` - interquartile range,
#'      \item `"range"` - range,
#'      \item `"cv"` - coefficient of variation,
#'      \item `"se"` - standard error of mean,
#'      \item `"skewness"` - skewness,
#'      \item `"kurtosis"` - excess kurtosis.
#'  }
#' @param na.rm (logical) Flag to remove missing values. Default is `TRUE`.
#' @param type (integer: 1, 2, 3) The type of skewness and kurtosis estimate.
#'             See [psych::describe()] and [psych::mardia()]
#'             for details.
#' @param trim The fraction (0 to 0.5) of observations to be trimmed from each end of sorted variable before the mean is computed. Values of trim outside that range are taken as the nearest endpoint.
#'
#' @return Data frame with summary satatistics.
#' @export
#'
#' @examples
#' library(biostat)
#' data(cabbages, package = "MASS")
#'
#' do_summary(~VitC, data = cabbages) %>%
#'   print(digits = 2)
#'
#' do_summary(VitC ~ Cult, data = cabbages) %>%
#'   print(digits = 2)
#'
#' do_summary(VitC ~ Cult + Date, data = cabbages, stat = "mean") %>%
#'   print(digits = 2)
#'
#' do_summary(HeadWt + VitC ~ Cult + Date,
#'   data = cabbages,
#'   stat = c("n", "mean", "sd")
#' ) %>%
#'   print(digits = 1)
#'
#'
#' # TODO:
#' # 1. First argument should be a data frame
#' #
do_summary <- function(
                       y,
                       data = NULL,
                       stat = c(
                         "n",
                         "missing",
                         "mean",
                         "trimmed",
                         "sd",
                         "variance",
                         "min",
                         "Q1",
                         "median",
                         "Q3",
                         "max",
                         "mad",
                         "IQR",
                         "range",
                         "cv",
                         "se",
                         "skewness",
                         "kurtosis"
                       ),
                       trim = 0.1,
                       type = 3,
                       na.rm = TRUE) {
  checkmate::assert_integerish(type, lower = 1, upper = 3)
  checkmate::assert_number(trim, lower = 0, upper = 0.5)
  checkmate::assert_logical(na.rm, any.missing = FALSE, len = 1)


  data_name <- substitute(data)

  if (is.null(data)) {
    data <- model.frame(y, rlang::f_env(y))
  }
  # Define functions with na.rm set to TRUE
  Q1 <- q1
  Q3 <- q3
  sd <- purrr::partial(stats::sd, na.rm = na.rm)
  variance <- purrr::partial(stats::var, na.rm = na.rm)
  mean <- purrr::partial(base::mean, na.rm = na.rm)
  trimmed <- purrr::partial(base::mean, na.rm = na.rm, trim = trim)
  median <- purrr::partial(stats::median, na.rm = na.rm)
  mad <- purrr::partial(stats::mad, na.rm = na.rm)
  max <- purrr::partial(base::max, na.rm = na.rm)
  min <- purrr::partial(base::min, na.rm = na.rm)
  IQR <- purrr::partial(stats::IQR, na.rm = na.rm)
  skewness <- purrr::partial(psych::skew, na.rm = na.rm, type = type)
  kurtosis <- purrr::partial(psych::kurtosi, na.rm = na.rm, type = type)

  cv <- function(x) sd(x) / mean(x)
  se <- function(x) sd(x) / sqrt(n_ok(x))
  range <- function(x) {
    diff(base::range(x, na.rm = na.rm))
  }


  # Extract numeric variables
  y_names <- all.vars(y[[2]])

  # Extract grouping variables
  gr_names <- if (length(y) == 3) {
    all.vars(y[[3]])
  } else {
    NULL
  }
  groups <- rlang::syms(gr_names)

  # Select statistics
  stat <- match.arg(stat, several.ok = TRUE)
  stat[stat == "n"] <- "n_ok"
  stat[stat == "missing"] <- "n_missing"
  stat_ <- rlang::syms(stat)

  # Function to reset names
  right_names <- function(y) {
    y[y == "n_ok"] <- "n"
    y[y == "n_missing"] <- "missing"
    # If only one stat, dplyr::summarise does
    # not write the name of the statistic. To correct this:
    if (length(stat) == 1) {
      y[length(y)] <- stat
    }
    y
  }

  # Define function for calculations
  calculate <- function(x_name) {
    num_x <- rlang::syms(x_name)

    data %>%
      dplyr::group_by(!!!groups) %>%
      dplyr::summarise_at(
        dplyr::vars(!!!num_x),
        dplyr::funs(!!!stat_)
      ) %>%
      dplyr::ungroup()
  }

  # Do the calculations
  (
    # if (length(y_names) > 1) {
    lapply(y_names, calculate) %>%
      setNames(y_names) %>%
      dplyr::bind_rows(.id = ".summary_of")

    # } else {
    #     calculate(y_names)
    # }
  ) %>%
    as.data.frame() %>%
    purrr::set_names(~ right_names(.)) %>%
    structure(
      class = c("num_summaries", "summaries_model", "data.frame"),
      vars = y_names,
      groups = gr_names,
      data = data_name
    )
}


#' @rdname do_summary
#'
#' @param x object to print
#' @param ... further arguments to methods.
#' @param digits_sk Number of digits for skweness and kurtosis.
#' @param digits Number of digits for descriptive statistics.
#' @inheritParams format_numbers
#' @export
print.num_summaries <- function(x, ..., digits = NA, format = "f", digits_sk = 2) {
  df <- format_numbers(
    data = as.data.frame(x),
    digits = c(
      mean = digits,
      trimmed = digits,
      sd = digits,
      variance = digits,
      min = digits,
      Q1 = digits,
      median = digits,
      Q3 = digits,
      max = digits,
      mad = digits,
      IQR = digits,
      range = digits,
      cv = digits, # ??? kiti matavimo vienetai nei x
      se = digits, # ??? kiti matavimo vienetai nei x
      skewness = digits_sk,
      kurtosis = digits_sk
    ),
    format = format
  )
  print(as.data.frame(df), ...)
}
