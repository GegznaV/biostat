#' @rdname posthoc_anova
#' @export
posthoc_anova_tukey <- function(y,
                                x = NULL,
                                conf_level = 0.95,
                                digits = 2,
                                p.adjust = "holm",
                                format_pvalue = TRUE,
                                data = NULL,
                                ...,
                                sep = " | ") {
  posthoc_anova(
    y = y,
    x = x,
    method = "tukey",
    conf_level = conf_level,
    digits = digits,
    p.adjust = p.adjust,
    format_pvalue = format_pvalue,
    data = data,
    ...,
    sep = " | "
  )
}

#' @rdname posthoc_anova
#' @export
posthoc_anova_games_howell <- function(y,
                                       x = NULL,
                                       conf_level = 0.95,
                                       digits = 2,
                                       p.adjust = "holm",
                                       format_pvalue = TRUE,
                                       data = NULL,
                                       ...,
                                       sep = " | ") {
  posthoc_anova(
    y = y,
    x = x,
    method = "games-howell",
    conf_level = conf_level,
    digits = digits,
    p.adjust = p.adjust,
    format_pvalue = format_pvalue,
    data = data,
    ...,
    sep = " | "
  )
}
