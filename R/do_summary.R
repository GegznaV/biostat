#' Do numerical summaries by groups
#'
#' Do numerical summaries by groups with formaula interface. Missing values are automatically removed.
#'
#' @param y formula with variable names to summarize. See more in examples.
#' @param data data set
#' @param stat (character) Descriptive statistics to compute. Currently supported statistics:
#'  \itemize{
#'      \item \code{"n"} - number of non-missing observations,
#'      \item \code{"missing"} - number of missing observations,,
#'      \item \code{"mean"} - arithmetic mean,
#'      \item \code{"sd"} - standard deviation,
#'      \item \code{"var"} - variance,
#'      \item \code{"min"} - minimum value,
#'      \item \code{"Q1"} - 1-st quartile,
#'      \item \code{"Md"} - median,
#'      \item \code{"Q3"} - 3-rd quartile,
#'      \item \code{"max"} - maximum value,
#'      \item \code{"IQR"} - interquartile range,
#'      \item \code{"skewness"} - skewness,
#'      \item \code{"kurtosis"} - excess kurtosis.
#'  }
#'
#'
#' @return Data frame with summary satatistics.
#' @export
#'
#' @examples
#' library(biostat)
#' data(cabbages, package = "MASS")
#'
#' do_summary(~VitC, data = cabbages) %>%
#'     print(digits = 2)
#'
#' do_summary(VitC ~ Cult, data = cabbages) %>%
#'     print(digits = 2)
#'
#' do_summary(VitC ~ Cult + Date, data = cabbages, stat = "mean") %>%
#'     print(digits = 2)
#'
#' do_summary(HeadWt + VitC ~ Cult + Date,
#'            data = cabbages,
#'            stat = c("n", "mean", "sd")) %>%
#'     print(digits = 1)
#'
do_summary <- function(
    y,
    data = NULL,
    stat = c("n",
             "missing",
             "mean",
             "sd",
             "var",
             "min",
             "Q1",
             "Md",
             "Q3",
             "max",
             "IQR",
             "skewness",
             "kurtosis")) {

    data_name <- substitute(data)

    if (is.null(data)) {
        data <- model.frame(y, rlang::f_env(y))
    }
    # Define functions with na.rm set to TRUE
          Q1 <- q1
          Q3 <- q3
          sd <- purrr::partial(stats::sd,      na.rm = TRUE)
         var <- purrr::partial(stats::var,     na.rm = TRUE)
        mean <- purrr::partial(base::mean,     na.rm = TRUE)
          Md <- purrr::partial(stats::median,  na.rm = TRUE)
         max <- purrr::partial(max,            na.rm = TRUE)
         min <- purrr::partial(min,            na.rm = TRUE)
         IQR <- purrr::partial(stats::IQR,     na.rm = TRUE)
    skewness <- purrr::partial(psych::skew,    na.rm = TRUE)
    kurtosis <- purrr::partial(psych::kurtosi, na.rm = TRUE)



    # Extract numeric variables
    y_names  <- all.vars(y[[2]])

    # Extract grouping variables
    gr_names <- if (length(y) == 3) {
        all.vars(y[[3]])
    } else {
        NULL
    }
    groups <- rlang::syms(gr_names)

    # Select statistics
    stat <- match.arg(stat, several.ok = TRUE)
    stat[stat == "n"      ] <- "n_ok"
    stat[stat == "missing"] <- "n_missing"
    stat_ <- rlang::syms(stat)

    # Function to reset names
    right_names <- function(y) {
        y[y == "n_ok"     ] <- "n"
        y[y == "n_missing"] <- "missing"
        # If only one stat, dplyr::summarise does
        # not write the name of the statistic. To correct this:
        if (length(stat) == 1)
            y[length(y)] <- stat
        y
    }

    # Define function for calculations
    calculate <- function(x_name) {
        num_x <- rlang::syms(x_name)

        data %>%
            dplyr::group_by(!!! groups) %>%
            dplyr::summarise_at(dplyr::vars(!!! num_x),
                                dplyr::funs(!!! stat_)) %>%
            dplyr::ungroup()
    }

    # Do the calculations
    (
        # if (length(y_names) > 1) {
            lapply(y_names, calculate) %>%
                setNames(y_names) %>%
                dplyr::bind_rows(.id = "summary_of")

        # } else {
        #     calculate(y_names)
        # }
    ) %>%
        as.data.frame() %>%
        purrr::set_names(~right_names(.))  %>%
        structure(class = c("num_summaries", "summaries_model", "data.frame"),
                  vars = y_names,
                  groups = gr_names,
                  data = data_name)

}


#' @rdname do_summary
#'
#' @param x object to print
#' @param ... further arguments to methods.
#' @inheritParams format_numbers
#' @export
print.num_summaries <- function(x, ..., digits = NA, format = "f") {
    df <- format_numbers(
        data =  as.data.frame(x),
        digits = c(mean = digits,
                   sd = digits,
                   var = digits,
                   min = digits,
                   Q1 = digits,
                   Md = digits,
                   Q3 = digits,
                   max = digits,
                   IQR = digits,
                   skewness = 2,
                   kurtosis = 2),
        format = format
    )
    print(as.data.frame(df), ...)
}
