context("gg_boxplot_plus")

test_that("`gg_boxplot_plus` works", {
  expect_silent(gg_boxplot_plus(decrease ~ treatment, data = OrchardSprays))
  expect_silent(gg_boxplot_plus(decrease ~ treatment, data = OrchardSprays, sort_groups = "ascending"))
  expect_silent(gg_boxplot_plus(decrease ~ treatment, data = OrchardSprays, sort_groups = "descending"))
})
