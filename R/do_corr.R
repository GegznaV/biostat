# TODO:
# 1. Show both p and p adjusted values.
# 2. rename parameter "R"
# 3. Review the function

#' do_corr
#'
#' @export
#' @examples
#' data(iris)
#' ans <- do_corr(iris[, -5])
#' ans
#'
#' library(pander)
#' pander(ans)
do_corr <- function(
                    x,
                    y = NULL,
                    method = c("spearman", "kendall", "pearson")[1],
                    use = "complete.cases", # [!!!] in the future allow pairwise complete cases
                    conf = 0.95,
                    R = 999,
                    sim = "balanced",
                    ci_type = c("bca"),
                    p_adjust_method = p.adjust.methods[1],
                    ss = p05plus) {

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ci_type <- match.arg(ci_type)
  p_adjust_method <- match.arg(p_adjust_method)
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (is.data.frame(x)) {
    df_corr <- x
  } else {
    df_corr <- data.frame(x = x, y = y)
  }
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Remove NA values
  cc <- complete.cases(df_corr)
  if (any(!cc)) {
    message(sum(!cc), " row(s) containing missing values were removed.")
    df_corr <- df_corr[cc, ]
  }
  # Number of observations
  n <- nrow(df_corr)
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  stopifnot("bca" %in% ci_type) # only "bca" is supported
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  v_boot <- boot::boot(
    df_corr,
    R = R,
    statistic = function(df, i) {
      corr_mat2vec(cor(df[i, ],
        # use = use,
        method = method
      ))
    }
  )
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  get_corr_ci <- function(i) {
    v_boot_ci <- boot::boot.ci(v_boot, conf = conf, type = ci_type, index = i)
    data.frame(
      "cor" = v_boot_ci$t0,
      "ci_low" = v_boot_ci$bca[4],
      "ci_upp" = v_boot_ci$bca[5]
    )
  }

  rez <- lapply(seq_along(v_boot$t0), get_corr_ci) %>%
    setNames(corr_mat2vec_names(cor(df_corr,
      # use = use,
      method = method
    ))) %>%
    dplyr::bind_rows(.id = "variables")

  # p value, two-sided alternative

  # TODO: igalinti kitus galimus p reikšmiu skaiciavimo metodus
  p <- switch(method,
    "pearson" = {
      2 * (1 - SuppDists::pPearson(abs(rez$cor), N = n))
    },
    "spearman" = {
      2 * (1 - SuppDists::pSpearman(abs(rez$cor), r = n))
    },
    # pspearman::spearman.test
    # pspearman::pspearman

    "kendall" = {
      2 * (1 - SuppDists::pKendall(abs(rez$cor), N = n))
    }
  )

  rez$p <- p
  rez$p.adj <- p.adjust(p, method = p_adjust_method)
  rez$n <- n
  rez$method <- method

  structure(rez,
    class = c("do_corr", "data.frame"),
    p_adjust_method = p_adjust_method,
    conf = conf,
    ci_type = ci_type,
    sim = sim,
    R = R,
    ss = ss
  )
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' @rdname do_corr
#' @export
print.do_corr <- function(x, ..., digits = 2, digits_p = 3, ss = attr(x, "ss"),
                          p_col = c("p", "p.adj")
                          # , p_col_rm = NULL
) {

  # x[[p_col_rm]] <- NULL

  x <- x %>%
    biostat::format_p_values(
      cols = p_col,
      digits_p = digits_p, ss = ss
    ) %>%

    # biostat::format_p_values(cols = p_col_rm,
    #                          digits_p = digits_p, signif_stars = FALSE) %>%

    biostat::format_numbers(c(
      cor = digits,
      ci_lower = digits,
      ci_upper = digits,
      ci_low = digits,
      ci_upp = digits
    ))

  NextMethod("print", x, ...)
}
#' @rdname do_corr
#' @export

# TODO: Padaryti, kad aprašyme `caption` rašytu visa pagrindine informacija
# apie analize (pvz., R = ..., method = "holm", type = "BCA", ...)
pander.do_corr <- function(x, ...,
                           caption = "The results of correlation analysis",
                           digits = 2, digits_p = 3, ss = attr(x, "ss"),
                           p_col = c("p", "p.adj")
                           # , p_col_rm = "p"
) {

  # x[[p_col_rm]] <- NULL
  x <- x %>%
    biostat::format_p_values(
      cols = p_col,
      digits_p = digits_p, ss = ss
    ) %>%

    # biostat::format_p_values(cols = p_col_rm,
    #                          digits_p = digits_p, signif_stars = FALSE) %>%

    biostat::format_numbers(c(
      cor = digits,
      ci_lower = digits,
      ci_upper = digits,
      ci_low = digits,
      ci_upp = digits
    ))

  NextMethod("pander", x, caption = caption, ...)
}
# ==============================================================================
get_lowtri <- function(obj) {
  obj[upper.tri(obj, diag = TRUE)] <- NA
  obj
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
corr_mat2vec <- function(x) {
  as.numeric(na.omit(as.vector(get_lowtri(x))))
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
corr_mat2vec_names <- function(x) {
  to_keep <- !is.na(as.vector(get_lowtri(x)))
  gr1 <- colnames(x)[col(x)][to_keep]
  gr2 <- rownames(x)[row(x)][to_keep]
  paste(gr1, gr2, sep = " - ")
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
