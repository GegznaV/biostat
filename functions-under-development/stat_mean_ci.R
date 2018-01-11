# A layer function.

stat_mean_ci <- function(mapping = NULL, data = NULL, geom = "errorbar",
                         position = "identity", na.rm = FALSE, show.legend = NA,
                         inherit.aes = TRUE, repetitions = 999,
                         conf_level = 0.95,  ...) {
    ggplot2::layer(
        stat = StatMeanCiY, data = data, mapping = mapping, geom = geom,
        position = position, show.legend = show.legend, inherit.aes = inherit.aes,
        params = list(na.rm = na.rm,  repetitions = repetitions, conf_level = conf_level, ...)
    )
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
StatMeanCiY <-
    ggproto("StatMeanCiY", Stat,

            required_aes = c("y"),
            default_aes = aes(ymin = ..ci_lower..,
                              ymax = ..ci_upper..),

            compute_group = function(data, scales,
                                     repetitions = 999,
                                     conf_level = 0.95) {

                # y <- na.omit(data$y)

                res <- biostat::ci_mean_boot(x,
                                             repetitions = repetitions,
                                             conf_level = conf_level)

                data.frame(x,
                           mean = res$mean,
                           ci_lower = res$lower,
                           ci_upper = res$upper)
            }
    )
