# ============================================================================
# TODO:
#
# [v] 1. Add parameters for xlab, ylab, legend_title = xlab.
# [ ] 2. Review documentation.
# [ ] 3. Allow formula of shape: ~ y
# [ ] 4. Allow formula of shape: ~ y | group
#
# [ ] 5. Convert groups to factors, if not a factor, and warn about this action.

#
# ============================================================================


#' [!] A boxplot with additional components
#'
#' A boxplot (in the style of Tukey) with additional components: mean and its confidence intervals as well as jittered points.
#'
#' The boxplot compactly displays the distribution of a continuous numeric variable. It visualises five summary statistics (including the first quartile, the median, and the third quartile) as well as all "outlying" points individually.
#'
#' The plot is based on \pkg{ggplot2}, thus \code{ggplot2} elements can be
#' added to modify the plot.
#'
#' @param formula a formula with two variable names to analyze. First one is numeric, second one is a factor, e.g. \code{y ~ group}.
#' @param data a data frame with data.
#' @param cld a data frame with cld results (object of class \code{cld_object}).
#' @param sort_groups (\code{"no"}|\code{"yes"}|\code{"ascending"}|\code{"descending"}) \cr
#'                    Sort groups by position of either median or other statistic indicated in \code{sort_fun}.
#' @param sort_fun A function that calculates one numeric statistic
#' (name without quotes). May be \code{median},
#' \code{mean}, \code{sd}, \code{var}, \code{IQR}, or similar.
#' @param add_points (\code{TRUE}|\code{FALSE})  \cr If \code{TRUE}, jittered point are added to the left of the boxplot.
#' @param add_mean_ci (\code{TRUE}|\code{FALSE})  \cr If \code{TRUE}, mean with its confidence interval are added to the right of the boxplot.
#' @param notch (\code{TRUE}|\code{FALSE})
#'  \cr if \code{FALSE} (default) make a standard box plot.
#'  If \code{TRUE}, notched boxplot is drawn. If the notches of two plots do not overlap this is a ‘strong evidence’ that the two medians statistically signigicantly differ (Chambers et al, 1983, p. 62).
#' @param conf_level (numeric) Confidence level for confidence interval. Number from 0 to 1. Default is 0.95.
#' @param ci_boot_reps (numeric) Number of bootstrap repetitions for mean confidence interval calculation.
#'
#' @param cld_color (character) \cr Name of color for cld letters.
#' @param cld_y_adj (numeric) \cr cly y position adjustment factor (additive) for cld letters.
#' @param cld_y_mult (numeric) \cr cly y position adjustment factor (multiplicative).
#'
#' @param ci_x_adj (numeric) \cr x position correction factor (additive) for mean confidence interval lines/dots.
#' @param points_x_adj (numeric) \cr x position correction factor (additive) for jittered points.
#' @param ... arguments to \code{sort_fun}.
#' @param xlab (character)  \cr Title for the x-axis.
#' @param ylab (character)  \cr Title the y-axis.
#' @param legend_title (character)  \cr Title for the legend.
#' @param varwidth (logical) \cr
#'        If \code{FALSE} (default) make a standard box plot.
#'        If \code{TRUE}, boxes are drawn with widths proportional to the
#'        square-roots of the number of observations in the groups.
#'
#' @seealso
#'
#' \url{https://en.wikipedia.org/wiki/Box_plot}
#'
#' @references
#'
#' Chambers, J. M., Cleveland, W. S., Kleiner, B. and Tukey, P. A. (1983) \emph{Graphical Methods for Data Analysis.} Wadsworth & Brooks/Cole.
#'
#'
#' @return A ggplot2 plot object.
#' @export
#'
#' @keywords ggplot2 plots
#'
#' @examples
#' library(biostat)
#'
#' # Example 1
#' gg_boxplot_plus(decrease ~ treatment, OrchardSprays)
#'
#'
#' # Example 2
#' gg_boxplot_plus(decrease ~ treatment, OrchardSprays,
#'                 sort_groups = "descending")
#'
#'
#' # Example 3a
#'
#' res <- posthoc_anova(weight ~ Diet, data = ChickWeight)
#' cld_result <- make_cld(res)
#'
#' gg_boxplot_plus(weight ~ Diet, data = ChickWeight,
#'                 cld = cld_result)
#'
#' # Example 3b
#'
#' gg_boxplot_plus(weight ~ Diet, data = ChickWeight,
#'                 cld = cld_result,
#'                 sort_groups = "descending",
#'                 sort_fun = mean)
#'
#' # Example 3c: do simple transformations
#'
#' gg_boxplot_plus(log(weight) ~ Diet, data = ChickWeight,
#'                 sort_groups = "descending",
#'                 sort_fun = mean)
#'
#' # Example 3d: facetting
#'
#' gg_boxplot_plus(weight ~ as.factor(Time), data = ChickWeight) +
#'     facet_wrap("Diet")
#'
#' # Example 4
#'
#' res2 <- posthoc_anova(decrease ~ treatment, data = OrchardSprays)
#' cld_result2 <- make_cld(res2)
#'
#' gg_boxplot_plus(decrease ~ treatment, data = OrchardSprays,
#'                 cld = cld_result2,
#'                 sort_groups = "descending",
#'                 sort_fun = mean)
#'
#' gg_boxplot_plus(decrease ~ treatment, data = OrchardSprays,
#'                 cld = cld_result2,
#'                 sort_groups = "ascending",
#'                 sort_fun = IQR)
#'
gg_boxplot_plus <- function(
    formula,
    data = NULL,
    cld = NULL,

    xlab = NULL,
    ylab = NULL,
    legend_title = NULL,

    sort_groups = c("no", "yes", "ascending", "descending"),
    sort_fun = median,

    add_points = TRUE,

    notch = FALSE,

    varwidth = FALSE,

    add_mean_ci = TRUE,
    conf_level = 0.95,
    ci_boot_reps = 999,

    cld_color = "black",
    cld_y_adj  = 0,
    cld_y_mult = 0.06,
    ci_x_adj = -0.3,
    points_x_adj =  0.3,

    ...

) {

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Prepare data
    if (!is.null(cld)) {
        checkmate::assert_class(cld, "cld_object")
    }

    # if (is.null(data)) {
    #     data <- rlang::f_env(formula)
    # }
    #
    # gr_name <- all.vars(formula[[3]])
    #    y_name <- all.vars(formula[[2]])
    #
    # DATA <- dplyr::select(data,
    #                       y = !!rlang::sym(y_name),
    #                       group = !! rlang::sym(gr_name))
    #
    # DATA <- dplyr::mutate(DATA, group = factor(group))

    obj <- parse_formula(formula, data, keep_all_vars = TRUE)
    y_name <- obj$names$y
    gr_name <- obj$names$x

    DATA <- dplyr::select(obj$data,
                          .y = !! rlang::sym(y_name),
                          .group = !! rlang::sym(gr_name),
                          dplyr::everything())

    desc <- switch(match.arg(sort_groups),
           "TRUE" = ,
           "yes" = ,
           "ascending" = FALSE,
           "descending" = TRUE,
           NULL)

    if (!is.null(desc)) {
        DATA <- dplyr::mutate(
            DATA,
            .group = forcats::fct_reorder(.group,
                                          .y,
                                          fun = sort_fun,
                                          ...,
                                          .desc = desc))
        # [!!!] Suggestion:
        #
        # y_name <- "weight"
        # y_sym <- rlang::sym(y_name)
        # gr <- "feed"
        #
        # mutate_at(chickwts, gr,
        #  .funs = forcats::fct_reorder,
        #   x = !! y_sym,
        #   fun = median,
        #   ...,
        #   .desc = desc)
    }

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Plot

    p <- ggplot(DATA, aes(x = .group, y = .y, fill = .group)) +
        geom_boxplot(width = .2, notch = notch, varwidth = varwidth)

    # p <- ggplot(DATA, aes(x = .group, y = .y, fill = .group)) +
    #     geom_violin(aes(color = .group), fill = NA, width = .6, lwd = 1) +
    #     geom_boxplot(width = .1, notch = notch)

    if (add_points) {
        p <- p +
            geom_jitter(
                aes(x = as.numeric(.group) + points_x_adj),
                alpha = 0.3,
                width = .08,
                shape = 21)
    }

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Add mean CI ------------------------------------------------------------
    if (add_mean_ci) {
        # mean_ci <- dplyr::do(dplyr::group_by(DATA, .group),
        #                      ci_mean_boot(.$.y, repetitions = ci_boot_reps,
        #                                    conf_level = conf_level))
        #
        # p <- p +
        #     geom_errorbar(data = mean_ci,
        #                   aes(x = as.numeric(.group) + ci_x_adj,
        #                       ymin = lower,
        #                       ymax = upper,
        #                       color = .group),
        #                   inherit.aes = FALSE,
        #                   width = 0.1) +
        #
        #     geom_point(data = mean_ci, shape = 21, color = "black",
        #                aes(x = as.numeric(.group) + ci_x_adj,
        #                    y = mean,
        #                    fill = .group),
        #                inherit.aes = FALSE)

        p <- p +
            stat_summary(aes(x = as.numeric(.group) + ci_x_adj,
                             color = .group),
                         geom = "errorbar",
                         fun.data = mean_cl_boot,
                         fun.args = list(
                             conf.int = conf_level,
                             B = ci_boot_reps),
                         width = 0.1) +

            stat_summary(aes(x = as.numeric(.group) + ci_x_adj,
                             color = .group),
                         geom = "point",
                         fun.y = mean)

    }

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Add cld, if provided ---------------------------------------------------
    if (!is.null(cld)) {

        y_upp <- max(DATA[[".y"]], na.rm = TRUE)
        y_low <- min(DATA[[".y"]], na.rm = TRUE)

        cld_y_pos <- cld_y_adj + y_upp + cld_y_mult*(y_upp - y_low)

        cld <- dplyr::mutate(cld,
                             group = factor(group, levels = levels(DATA$.group)))

        p <- p +
            geom_text(data = cld,
                      color = cld_color,
                      aes(x = group, label = cld, y = cld_y_pos),
                      fontface = "bold",
                      inherit.aes = FALSE)
    }

    # Add labels -------------------------------------------------------------
    if (is.null(ylab))                 ylab <- y_name
    if (is.null(xlab))                 xlab <- gr_name
    if (is.null(legend_title)) legend_title <- gr_name

    p <- p +
        labs(x = xlab, y = ylab, fill = legend_title, color = legend_title) +
        theme_bw()

    # Output -----------------------------------------------------------------
    p
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
vertical_cld <- function(iii) {
    iii <- as.character(iii)
    # iii <- gsub("_", " ", iii)
    sapply(iii, function(x) paste(strsplit(x,"")[[1]], "\n", collapse = ""))
}


# geom_text(data = cld_result,
#           aes(x = group, label = vertical_cld(spaced_cld), y = cld_y_),
#           fontface = "bold",
#           inherit.aes = FALSE,
#           vjust = 0) +
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

