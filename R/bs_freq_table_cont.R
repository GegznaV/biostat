# TODO:
#
# 1 **[!!!]**
#
#
#' Frequency table for continuous data
#'
#' @param y (numeric) A numeric vector with data.
#' @param nbins (integer) Number of bins.
#' @param bin_width (number) Bin width.
#' @param xmin (number) Lower position of the lowest bin. Not bigger than `min(x)`.
#' @param xmax (number) Upper position of the highest bin. Not smaller than `max(x)`.
#' @param max_bin_decimals (integer) Maximum number of significant decimal digits to display in bin names.
#' @param percent_decimals (integer) Number of decimal numbers in percentages.
#' @param right (logical) Flag that indicates if the intervals should be closed
#'              on the right and open on the left (default) or vice versa.
#'
#' @return Object of classes `bs_freq_table_cont`, `data.frame`.
#' @export
#'
#' @examples
#' library(biostat)
#' library(pander)
#' bs_freq_table_cont(iris$Sepal.Length, nbins = 5)
#' bs_freq_table_cont(iris$Sepal.Length, bin_width = 1, xmin = 4)
#' bs_freq_table_cont(iris$Sepal.Length, nbins = 5) %>% pander()
#'
#' # [!!!BLOGAI]:
#' # bs_freq_table_cont(iris$Sepal.Length, bin_width = 5, xmax = 10)
bs_freq_table_cont <- function(
                               y,
                               nbins = NULL,
                               bin_width = NULL,
                               xmin = NULL,
                               xmax = NULL,
                               max_bin_decimals = 2,
                               percent_decimals = 1,
                               right = TRUE) {
  # Check input
  if (!xor(is.null(nbins), is.null(bin_width))) {
    if (is.null(nbins)) {
      stop("You must enter either `bin_width` or `nbins`.")
    } else {
      stop("You must enter either `bin_width` or `nbins` but not both.")
    }
  }

  checkmate::assert_numeric(y)
  checkmate::assert_integerish(nbins, lower = 1, null.ok = TRUE)
  checkmate::assert_number(bin_width, lower = 0, null.ok = TRUE)
  checkmate::assert_number(xmin, upper = min(y, na.rm = TRUE), null.ok = TRUE)
  checkmate::assert_number(xmax, lower = max(y, na.rm = TRUE), null.ok = TRUE)
  checkmate::assert_integerish(max_bin_decimals)
  checkmate::assert_logical(right)

  # Set defaults
  if (is.null(xmin)) {
    xmin <- min(y, na.rm = TRUE)
  }

  if (is.null(xmax)) {
    xmax <- max(y, na.rm = TRUE)
  }

  # Calculate position of bin breaks
  if (!is.null(nbins)) {
    bin_breaks <- seq(from = xmin, to = xmax, length.out = nbins + 1)
    # bin_width <- (xmax - xmin) / nbins
  } else if (!is.null(bin_width)) {
    bin_breaks <- seq(from = xmin, to = xmax + bin_width, by = bin_width)
    # nbins <- length(bin_breaks) - 1
  }

  # Avoid too many decimal digits
  dec <- max(n_decimals_max(signif(bin_breaks,
    digits = max_bin_decimals
  )))

  bin_labels <- sprintf(
    glue::glue("%.{dec}f -- %.{dec}f"),
    bin_breaks[-(length(bin_breaks))],
    bin_breaks[-1]
  )

  percent_fmt <- glue::glue("%.{percent_decimals}f %%")

  # Calculate table
  cut(y,
    breaks = bin_breaks,
    include.lowest = TRUE,
    labels = bin_labels,
    right = right
  ) %>%
    table() %>%
    as.data.frame() %>%
    dplyr::rename(Bin = ".") %>%
    dplyr::mutate(
      `Cum_freq` = cumsum(Freq),
      `Percent` = sprintf(
        percent_fmt,
        Freq / sum(Freq) * 100
      ),
      `Cum_percent` = sprintf(
        percent_fmt,
        `Cum_freq` / sum(Freq) * 100
      )
    ) %>%
    structure(class = c("bs_freq_table_cont", "data.frame"))
  # format_numbers(digits = c(`Percent` = 1, `Cum perc` = 1)) %>%
}


#' @rdname bs_freq_table_cont
#'
#' @param x object to print
#' @param ... further arguments to methods
#' @param caption (string) caption
#' @param justify (string) Column justification
#' @param style (string) the name of style for table.
#'
#' @export

pander.bs_freq_table_cont <- function(x, ..., caption = "Frequency table", justify = c("crrrr"), style = "simple") {
  pander::pander(as.data.frame(x),
    caption = caption,
    justify = justify,
    style = style,
    ...
  )
}
