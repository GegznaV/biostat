
# group.CI <-
# function (x, data, ci = 0.95)
# {
#     return(group.UCL(x, data, FUN = CI, ci = ci))
# }
# # <environment: namespace:Rmisc>
#
#
#
# group.UCL <-
# function (x, data, FUN, ...)
# {
#     d <- aggregate(x, data, FUN = FUN, ...)
#     y <- colnames(d)[ncol(d)]
#     n <- as.data.frame(d[, y])
#     colnames(n) <- sapply(list("upper", "mean", "lower"), function(l) {
#         paste(y, l, sep = ".")
#     })
#     d[ncol(d)] <- NULL
#     return(cbind(d, n))
# }
# # <environment: namespace:Rmisc>
#
#
# aggregate(formula, data, FUN = FUN, ...)
#


# # x = weight, group = feed, data = weight
# # data1 <- with(chickwts, tapply(weight, feed, qq_data))
#
#      x <- chickwts$weight
# groups <- chickwts$feed
#
# qq_data_by_group <-
#     function(x, group = NULL, data = NULL, ...) {
#
#         x     <- getVarValues(x, data)
#         group <- getVarValues(group, data)
#
#         tapply(x, group, qq_data)  %>%
#             do.call(rbind, .) %>%
#             dplyr::mutate(ID = rownames(.))  %>%
#             tidyr::separate(ID, "ID", sep = "\\.\\d*$", extra = "drop")
#     }
#
# qq_data_by_group("weight", "feed", chickwts)

# ==============================================================================



# library(biostat)
# library(PMCMR)
#
# model_tukey <- posthoc.kruskal.conover.test(decrease ~ treatment, data = OrchardSprays)
#
# model_tukey
#
# cld_result <- make_cld(model_tukey)
# cld_result
# pander::pander(cld_result)
#
# library(tidyverse)

# OrchardSprays
# cld_max <- with(OrchardSprays, tapply(decrease, treatment, function(x) 1.05 * max(x)))
# cld_y <- max(-OrchardSprays$decrease * 0.95)
# cld_y <- min(-OrchardSprays$decrease * 1.05)
#
# cld_y <- min(OrchardSprays$decrease * 0.95)
# cld_y <- max(OrchardSprays$decrease * 1.05)






#
# library(tidyverse)
#
# cld = cld_result
# add_points = TRUE
# add_mean_ci = TRUE
# add_median_ci = FALSE
#
# cld_y_adj = 1.05
# ci_x_adj = -0.3
# ci_boot_reps = 2000
#
# points_x_adj =  0.3
