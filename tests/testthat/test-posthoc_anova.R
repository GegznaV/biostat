context("posthoc_anova")

# **[!!!]** A more reliable unit test is needed
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
test_that("`posthoc_anova`, method = 'Games-Howell' works", {
  data(ChickWeight)
  rez1 <- posthoc_anova(
    y = ChickWeight$weight,
    group = ChickWeight$Diet,
    method = "Games-Howell"
  )
  expect_is(unclass(rez1), "list")


  rez2 <- posthoc_anova(weight ~ Diet,
    data = ChickWeight,
    method = "Games-Howell"
  )
  expect_is(unclass(rez2), "list")
})

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
test_that("`posthoc_anova`, method = 'Tukey' works", {
  data(ChickWeight)

  rez1 <- posthoc_anova(
    y = ChickWeight$weight,
    group = ChickWeight$Diet,
    method = "Tukey"
  )
  expect_is(unclass(rez1), "list")

  rez2 <- posthoc_anova(weight ~ Diet,
    data = ChickWeight,
    method = "Tukey"
  )
  expect_is(unclass(rez2), "list")
})
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
