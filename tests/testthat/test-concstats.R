local_edition(3)

# concstats_concstats

test_that("concstats_concstats function operates properly", {

  x <- c(0.2, 0.3, 0.4, 0.1)
  x1 <- c(0.2, 0.3, 0.4, -0.1)
  x2 <- c(0.4, 0.3, 0.2, 0.1, NA)
  x3 <- c(0.4, 0.3, 0.2, 0.1)

  expect_true(any(is.na(x2)), all(!is.na(x2)))
  expect_true(all(round(x3) == 0), (abs(x3) > 0 & abs(x3) <= 1))
  expect_vector(x3, ptype = double(), size = 4)
  expect_true(is.numeric(x3), label = "numeric values returned")
  expect_equal(concstats_concstats(x2, na.rm = FALSE), NA_integer_)
  expect_equal(sort(x, decreasing = TRUE), x3)
  expect_error(concstats_concstats(x1, !isTRUE(all.equal(1, sum(x),
                                          tolerance = .Machine$double.eps^0.25))),
               "vector does not sum to 1")
})

test_that("concstats_firm returns number of firms", {

  x <- c(0.4, 0.3, 0.2, 0.1)

  expect_equal(concstats_firm(x), sum(x > 0))
})

test_that("concstats_nrs_equ returns numbers equivalent", {

  x <- c(0.4, 0.3, 0.2, 0.1)

  expect_equal(concstats_nrs_eq(x), 1 / sum(x ^ 2))
})

test_that("concstats_top returns top market share", {

  x <- c(0.4, 0.3, 0.2, 0.1)

  expect_equal(concstats_top(x), x[1] * 100)
})

test_that("concstats_top3 returns sum of top 3 market shares", {

  x <- c(0.4, 0.3, 0.2, 0.1)

  expect_equal(concstats_top3(x), sum(x[1:3], na.rm = TRUE) * 100)
})

test_that("concstats_top5 returns sum of top 5 market shares", {

  x <- c(0.4, 0.2, 0.15, 0.15, 0.1)

  expect_equal(concstats_top5(x), sum(x[1:5], na.rm = TRUE) * 100)
})

test_that("concstats_hhi returns sum of squared shares as decimal", {

  x <- c(0.4, 0.2, 0.15, 0.15, 0.1)

  expect_equal(concstats_hhi(x), sum(x ^ 2))
})

test_that("concstats_entropy returns the entropy measure", {

  x <- c(0.4, 0.2, 0.15, 0.1, 0.05, 0.07, 0.03)

  expect_equal(concstats_entropy(x), (sum(-x / sum(x) * log(x / sum(x), base = 2))
                                      / log(sum(x > 0), base = 2)))
})

  test_that("concstats_palma returns the alternative palma inequality measure", {

  x <- c(0.2, 0.3, 0.5)

  expect_equal(concstats_palma(x),
               sum(x
                   [cut(x, stats::quantile(x, probs = seq(0, 1, 0.1)),
                        include.lowest = TRUE, labels = FALSE) > 9]) /
                 sum(x
                     [cut(x, stats::quantile(x, probs = seq(0, 1, 0.1)),
                          include.lowest = TRUE, labels = FALSE) <= 4]))
})

test_that("concstats_concstats returns a data frame", {

  x <- c(0.4, 0.2, 0.15, 0.1, 0.05, 0.07, 0.03)

  expect_true(is.data.frame(concstats_concstats(x)), "data.frame")
})
