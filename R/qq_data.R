#' [!] Compute data for a qq-plot
#'
#' Compute data necessary to plot a quantile comparison plot (qq-plot).
#'
#' @details
#' Function \code{qq_data} is inspired by \code{qqPlot()} in package \pkg{car}
#' (writen by J. Fox).
#'
#' @param envelope (numeric | \code{FALSE}) confidence level for point-wise
#'                 confidence envelope.
#' @param line (string) A parameter, that controls how reference line is drawn.
#'            Options:\itemize{
#'     \item{\code{"0,1"} or \code{"int=0,slope=1"} to plot a line
#'             with intercept = 0 and slope = 1;}
#'     \item{ \code{"quartiles"} to pass a line through the quartile-pairs;}
#'     \item{ \code{"robust"} for a robust-regression line;
#'             the latter uses the \code{rlm} function from the \pkg{MASS} package};
#'     \item{option \code{"none"} is not implemented yet.}
#' }
#'
#' @param method (string: \code{"trimmed-normal"}, \code{"normal"},
#'              \code{"any"}). A method, that controls how estimates of
#'              parameters for \code{distribution} are computed.
#'      Options:
#'      \itemize{
#'
#'            \item{\code{"mle-normal"} (default) all data values are used to
#'            estimate parameters of theoretical normal distribution using
#'            method of maximum likelihood;}
#'
#'            \item{\code{"trimmed-normal"} 10\% of the most extreme
#'            data values are trimmed before parameters of theoretical normal
#'            distribution are estimated using method of moments;}
#'
#'            \item{\code{"moment-normal"} all data values are used to estimate
#'            parameters of theoretical normal distribution using method of moments;}
#'
#'            \item{\code{"any"} either parameters are provided manually by user
#'            or default parameters are used (if any).}
#'      }
#'           Options \code{"mle-normal"}, \code{"trimmed-normal"} and
#'           \code{"moment-normal"} are applicable only if
#'           \code{distribution = "norm"}.
#'           Otherwise \code{"any"} is selected automatically.
#'
#' @param sep (not used yet).
#'
#' @param ... Parameters to be passed to function, selected in \code{distribution}.
#'            In \code{print} method, further parameters to function \code{print}.
#'
#' @inheritParams test_normality
#' @inheritParams car::qqPlot
#'
#' @return An object, which inherits from classes \code{qqdata} and
#'         \code{data.frame}. The object contains information, needed
#'         to plot a qqplot with reference line and its confidence intervals.
#'         These variables are contained: \itemize{
#'         \item\strong{x} – x axis values;
#'         \item\strong{y} – y axis values for points of qq plot;
#'         \item\strong{labels} – labels for each point;
#'         \item\strong{ref_y} – y axis values for reference line
#'         \item\strong{ref_ci_upper} and \strong{ref_ci_lower}
#'           – y axis values for upper and lower pointwise
#'            confidence interval of a reference line.
#'         }
#' @export
#'
#' @examples
#'
#' library(biostat)
#' data(chickwts, package = "datasets")
#'
#' # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' # Input as formula + data:
#'
#' my_qq_df <- qq_data(~weight, data = chickwts)
#' head(my_qq_df)
#' coef(my_qq_df)
#'
#' # Column ".group" is added if applied by group:
#'
#' my_qq_df <- qq_data(weight ~ feed, data = chickwts)
#' head(my_qq_df)
#' coef(my_qq_df)
#'
#' qq_plot(weight ~ feed, data = chickwts)
#'
#' # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' # Input as variable name + data:
#'
#' my_qq_df <- qq_data("weight", data = chickwts)
#' head(my_qq_df)
#' coef(my_qq_df)
#'
#' my_qq_df <- qq_data("weight",groups = "feed", data = chickwts)
#' head(my_qq_df)
#' coef(my_qq_df)
#'
#' # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' # Input as vector
#'
#' my_qq_df <- qq_data(chickwts$weight)
#' head(my_qq_df)
#' coef(my_qq_df)
#'
#' # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' # Input as vector, several groups.
#' # Column ".group" is added
#'
#' my_qq_df <- qq_data(chickwts$weight, groups = chickwts$feed)
#' head(my_qq_df)
#' coef(my_qq_df)
#'
#'
#'
#' @seealso \code{\link[car]{qqPlot}} in \pkg{car} package,
#'  \code{\link[stats]{qqplot}} in \pkg{stats} package.
#'

qq_data <- function(y,
                    data = NULL,
                    distribution = "norm",
                    ...,
                    line = c("quartiles", "robust", "int=0,slope=1", "0,1", "none"),
                    envelope = 0.95,
                    method = c("mle-normal", "trimmed-normal", "moment-normal", "any"),
                    labels = NULL,
                    groups = NULL,
                    sep = " | ")
{
    UseMethod("qq_data")
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @export
#' @importFrom purrr "%||%"
qq_data.default <- function(y,
                     data = NULL,
                     distribution = "norm",
                     ...,
                     line = c("quartiles", "robust", "int=0,slope=1", "0,1", "none"),
                     envelope = 0.95,
                     method = c("mle-normal","trimmed-normal","moment-normal", "any"),
                     labels = NULL,
                     groups = NULL,
                     sep = " | "
)

{
    y      <- getVarValues(y, data)
    groups <- getVarValues(groups, data)

    names(y) <-
        if (is.null(labels)) {
            as.character(names(y) %||% seq(along = y))

        } else {
            labels
        }

    method <- method[1]
    method <- match.arg(method)

    if (method == "trimmed-normal" & distribution == "norm") {
        qq_fun <- function(y){
            labels <- names(y)
            qq_data_(
                y = y,
                distribution = distribution,
                mean =           mean(y, trim = 0.1),
                sd   = stats::sd(trim(y, trim = 0.1)),
                envelope = envelope,
                line   = line,
                labels = labels
            )
        }

    } else if (method == "moment-normal" & distribution == "norm") {
        qq_fun <- function(y){
            labels <- names(y)
            qq_data_(
                y = y,
                distribution = distribution,
                mean = mean(y),
                sd   = stats::sd(y),
                envelope = envelope,
                line   = line,
                labels = labels
            )
        }

    } else if (method == "mle-normal" & distribution == "norm") {
        qq_fun <- function(y){
            labels <- names(y)
            params <- fitdistrplus::fitdist(y, "norm") %>% stats::coef()
            qq_data_(
                y = y,
                distribution = distribution,
                mean = params["mean"],
                sd   = params["sd"],
                envelope = envelope,
                line   = line,
                labels = labels
            )
        }

    } else {
        qq_fun <- function(y){
            labels <- names(y)
            qq_data_(
                y = y,
                distribution = distribution,
                ...,
                envelope = envelope,
                line   = line,
                labels = labels
            )
        }
    }

    # If no groups exist
    if (is.null(groups)) {
        DF <- qq_fun(y)

    # Applied by group
    } else {
        old_levels <- levels(groups)
        DF <- tapply(y, groups, qq_fun)

        # Make data frames to match a output in single group
        # DF_attr <-
        #     purrr::map(DF, ~ attributes(.x)$refline)  %>%
        #     rbind_df_in_list()
        DF_attr_refline <-
            purrr::map(DF, ~ attributes(.x)$refline)  %>%
            dplyr::bind_rows(.id = ".group")
        # %>%
            # dplyr::mutate(.group = factor(.group, levels = old_levels))

        DF_attr_params <-
            purrr::map(DF, ~ attributes(.x)$params)  %>%
            dplyr::bind_rows(.id = ".group")
        # %>%
            # dplyr::mutate(.group = factor(.group, levels = old_levels))

        DF %<>% rbind_df_in_list()
        # %>%
            # dplyr::mutate(.group = factor(.group, levels = old_levels))

        DF %<>% structure(
                  refline = DF_attr_refline,
                  params = DF_attr_params)
    }

    # Return
    structure(DF,
              class = c("qqdata", "data.frame"),
              distribution = distribution)
}


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @export
# @rdname qq_data
qq_data.formula <- function(
    y,
    data = NULL,
    distribution = "norm",
    ...,
    line = c("quartiles", "robust", "int=0,slope=1", "0,1", "none"),
    envelope = 0.95,
    method = c("mle-normal", "trimmed-normal", "moment-normal", "any"),
    labels = NULL,
    groups = NULL,
    sep = " | "
)
{
    # [!!!] qq_data.formula method needs revision
    DF <- model.frame(y, data = data)

    qq_main <- function(y,  groups = NULL){
        qq_data(
            y = y,
            distribution = distribution,
            ...,
            envelope = envelope,
            line   = line,
            labels = labels,
            groups = groups,
            method = method
        )
    }

    if (length(y) == 2) {
        rez <- qq_main(DF[[1]])

    } else if (length(y) > 2) {
        rez <- qq_main(DF[[1]], groups = interaction(DF[-1], sep = sep))

    } else {
        stop(
            "\nThe formula (`", y,"`) is incorrect.",
            "\nIt must contain at least 1 variable name."
        )
    }
    rez
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
qq_data_ <- function(y,
                     distribution = "norm",
                     ...,
                     line = c("quartiles", "robust", "int=0,slope=1", "0,1", "none"),
                     envelope = 0.95,
                     labels = if (!is.null(names(y))) names(y) else seq(along = y))

{
    line <- line[1]
    if (is.null(line) | isTRUE(line)) {
        line <- "none"
    }

    line <- match.arg(line)

    good <- !is.na(y)
    ord  <- order(y[good])

    ord_x   <- y[good][ord]
    ord_lab <- labels[good][ord]
    q_function <- eval_glue("q{distribution}")
    d_function <- eval_glue("d{distribution}")
    n <- length(ord_x)
    P <- ppoints(n)
    z <- q_function(P, ...)

    # Define a reference line
    switch(line,

           "quartiles" = {
               probs <- c(0.25, 0.75)

               Q_x <- stats::quantile(ord_x, probs)
               Q_z <- q_function(probs, ...)

               b <- (Q_x[2] - Q_x[1]) / (Q_z[2] - Q_z[1])
               a <- Q_x[1] - b * Q_z[1]

           },

           "robust" = {
               coef <- stats::coef(MASS::rlm(ord_x ~ z))
               a <- coef[1]
               b <- coef[2]

           },

           "0,1" = ,
           "int=0,slope=1" = {
               a <- 0
               b <- 1
               line <- "int=0,slope=1"
           },

           "none" = {
               # Line will not be plotted
               a <- 0
               b <- 1
               envelope <- FALSE
               # warning('Line will not be plotted (`line = "none"`)')
           }
    )

    ## !!! may be a mistake in `if (envelope == FALSE) 0.95`. Is `TRUE` correct?
    if (envelope == TRUE) {
        conf <- 0.95
    } else if (envelope == FALSE) {
        conf <- 0.95
    } else {
        conf <- envelope
        envelope <- TRUE
    }

    # Pointwise confidence interval
    zz <- qnorm(1 - (1 - conf) / 2)
    SE <- (b / d_function(z, ...)) * sqrt(P * (1 - P) / n)
    fit_value <- a + b * z
    upper <- fit_value + zz * SE
    lower <- fit_value - zz * SE

    # Output -------------------------------------------------------------
    data_points <-
        data.frame(
            x = z,
            y = ord_x,
            labels = ord_lab,
            ref_y  = fit_value,
            ref_ci_upper = upper,
            ref_ci_lower = lower
        )

    qq_attributes <- data.frame(
        intercept = a,
        slope = b,
        refline_type  = line,
        plot_envelope = envelope, # TRUE | FALSE
        conf = conf
    )
    rownames(qq_attributes) <- "qq_refline"

    # return
    structure(data_points,
              refline = qq_attributes,
              distribution = distribution,
              params = data.frame(..., row.names = NULL))

    # attr(data_points, "refline") <- qq_attributes
    # return(data_points)
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# =============================================================================
#' @param object A \code{qqdata} object.
#' @export
#' @method coef qqdata
#' @rdname qq_data
coef.qqdata <- function(object, ...) {
    attributes(object)$refline
}

# ============================================================================
# A method to plot a `qqdata` object as a qq-plot

#' @rdname qq_data
#' @param x A \code{qqdata} object.
#' @param scales ("free"|"free_x"|"free_y"|"fixed")
#'               a parmeter to be passed to
#'                \code{\link[ggplot2]{facet_wrap}}.
#' @param use_colors (logical) use colors for multiple groups
#' @import ggplot2
#' @examples
#' library(biostat)
#' data(chickwts, package = "datasets")
#'
#' # Input as formula + data:
#'
#' my_qq_df <- qq_data(weight ~ feed, data = chickwts)
#' plot(my_qq_df)
#'
#' my_qq_df2 <- qq_data(weight ~ feed, data = chickwts, method = "moment-normal")
#' plot(my_qq_df2)
#'
#' my_qq_df3 <- qq_data(weight ~ feed, data = chickwts, method = "any")
#' plot(my_qq_df3)
#'
#'
#' # The same x and y scale limits for all plots
#' plot(my_qq_df, scales = "fixed")
#'
#' # Plot in color
#' plot(my_qq_df, use_colors = TRUE)
#'
#' # Plot a qq-plot (with no grouping)
#' qq_single <- qq_data(~weight, data = chickwts)
#' plot(qq_single)
#'
#' class(qq_single)
#'
#'
#' # More than one grouping variable
#' data(CO2, package = "datasets")
#'
#'  qq_co2 <- qq_data(uptake ~ Type + Treatment, data = CO2)
#'  plot(qq_co2)
#'
#'  qq_co2_B <- qq_data(uptake ~ Type + Treatment, data = CO2, line = "robust")
#'  plot(qq_co2_B)
#'
#' @importFrom graphics plot
#' @export

plot.qqdata <- function(x,
                        ...,
                        use_colors = FALSE,
                        scales = "free"
                        )

{
    if (".group" %in%  colnames(x)) {
        if (use_colors) {
            p <- ggplot(x,
                        aes_string(
                            "x", "y",
                            ymin = "ref_ci_lower",
                            ymax = "ref_ci_upper",
                            col  = ".group",
                            fill = ".group"
                        ))
        } else {
            p <- ggplot(x,
                        aes_string("x", "y",
                            ymin = "ref_ci_lower",
                            ymax = "ref_ci_upper"))
        }

        p <- p + facet_wrap(".group", scales = scales)

    } else {
        p <- ggplot(x,
                    aes_string("x", "y",
                        ymin = "ref_ci_lower",
                        ymax = "ref_ci_upper"))
    }

    # Should reference line be plotted
    if (coef(x)$refline_type[1] != "none") {
        p <- p +
            geom_line(aes_string(y = "ref_y"), lty = 2)

    }

    refline_type <- coef(x)$refline_type[1]
    envelope_ <- ifelse(coef(x)$plot_envelope[1],
                        yes = paste("; conf.int.:", coef(x)$conf[1]),
                        no  = "")

    distrib_text <-
        switch(attr(x, "distribution"),
               norm = "normal",
               t = "t",
               f = "F",
               attr(x, "distribution")
        )

    p <- p +
        geom_point() +

        labs(x = glue::glue("Theoretical {distrib_text} quantiles"),
             y = "Empirical quantiles",
             color = "",
             fill = "") +
        labs(title = "QQ plot" ,
             subtitle = glue::glue("Ref.line: {refline_type}{envelope_}"))

    if (coef(x)$plot_envelope[1] != FALSE) {
        p <- p +
            geom_ribbon(alpha = 0.2, col = NA) +
            geom_line(aes_string(y = "ref_ci_lower"), lty = 2) +
            geom_line(aes_string(y = "ref_ci_upper"), lty = 2)
    }

    # Output:
    p
}

# =============================================================================

