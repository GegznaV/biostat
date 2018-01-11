context("make_cld")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
test_that("`make_cld.pairwise.htest` works", {
    obj1 <- pairwise.wilcox.test(chickwts$weight,
                                 chickwts$feed,
                                 exact = FALSE)
    expect_equivalent(as.character(make_cld(obj1)$cld),
                      c("a", "b", "bc", "ac", "c", "a"))
})

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
test_that("`make_cld.PMCMR` works", {
    expect_warning(
    obj2 <- PMCMR::posthoc.kruskal.conover.test(weight ~ feed,
                                                data = chickwts)
    )

    expect_equivalent(as.character(make_cld(obj2)$cld),
                      c("a", "b", "bc", "ac", "c", "a"))
})

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
test_that("`make_cld.pairwise.htest` works with insignificant results", {
    smokers  <- c(83, 90, 129, 70)
    patients <- c(86, 93, 136, 82)
    expect_warning(obj3 <- pairwise.prop.test(smokers, patients))

    expect_equivalent(as.character(make_cld(obj3)$cld), c("a", "a", "a", "a"))
})
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Test for class `posthocTGH` is needed.

# test_that("`make_cld.posthocTGH` works", {
#     # Temporary test
#     obj10 <- userfriendlyscience::posthocTGH(chickwts$weight, chickwts$feed)
#     expect_equivalent(as.character(make_cld(obj10)$cld),
#                       c("a","ab","bc","b","c","c"))
#
#     expect_equivalent(as.character(make_cld(obj10)$group),
#                       c("horsebean", "linseed", "meatmeal", "soybean", "sunflower", "casein" ))
# })

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
test_that("`make_cld.posthoc_anova` works", {
    obj9 <- posthoc_anova(chickwts$weight, chickwts$feed)
    expect_equivalent(as.character(make_cld(obj9)$cld),
                      c("a", "b","bc","ac","c","a"))

    expect_equivalent(as.character(make_cld(obj9)$group),
                      c("casein", "horsebean", "linseed", "meatmeal", "soybean", "sunflower"))
})

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
test_that("`make_cld.matrix` (symetric) works", {
    m <- c(1.00, 0.22, 0.05, 0.00,
           0.22, 1.00, 0.17, 0.01,
           0.05, 0.17, 1.00, 0.22,
           0.00, 0.01, 0.22, 1.00)
    obj7 <- matrix(m, nrow = 4)
    rownames(obj7) <- colnames(obj7) <- c("P", "O", "I", "U")

    expect_equivalent(as.character(make_cld(obj7)$cld),
                      c("a", "a", "ab", "b"))
})
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~