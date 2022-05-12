local_edition(3)

# concstats

test_that("concstats function operates properly", {

  x <- c(0.2, 0.3, 0.4, 0.1)
  x1 <- c(0.2, 0.3, 0.4, -0.1)
  x2 <- c(0.4, 0.3, 0.2, 0.1, NA)
  x3 <- c(0.4, 0.3, 0.2, 0.1)

  expect_true(any(is.na(x2)), all(!is.na(x2)))
  expect_true(all(round(x3) == 0), (abs(x3) > 0 & abs(x3) <= 1))
  expect_vector(x3, ptype = double(), size = 4)
  expect_true(is.numeric(x3), label = "numeric values returned")
  expect_equal(concstats(x2, na.rm = FALSE), NA_integer_)
  expect_equal(sort(x, decreasing = TRUE), x3)
  expect_error(firm(x1, !isTRUE(all.equal(1, sum(x),
                                          tolerance = .Machine$double.eps^0.25))),
               "vector does not sum to 1")
})

test_that("firm returns number of firms", {

  x <- c(0.4, 0.3, 0.2, 0.1)

  expect_equal(firm(x), sum(x > 0))
})

test_that("nrs_equ returns numbers equivalent", {

  x <- c(0.4, 0.3, 0.2, 0.1)

  expect_equal(nrs_eq(x), 1 / sum(x ^ 2))
})

test_that("top returns top market share", {

  x <- c(0.4, 0.3, 0.2, 0.1)

  expect_equal(top(x), x[1] * 100)
})

test_that("top3 returns sum of top 3 market shares", {

  x <- c(0.4, 0.3, 0.2, 0.1)

  expect_equal(top3(x), sum(x[1:3], na.rm = TRUE) * 100)
})

test_that("top5 returns sum of top 5 market shares", {

  x <- c(0.4, 0.2, 0.15, 0.15, 0.1)

  expect_equal(top5(x), sum(x[1:5], na.rm = TRUE) * 100)
})

test_that("hhi returns sum of squared shares as decimal", {

  x <- c(0.4, 0.2, 0.15, 0.15, 0.1)

  expect_equal(hhi(x), sum(x ^ 2))
})

test_that("entropy returns the entropy measure", {

  x <- c(0.4, 0.2, 0.15, 0.1, 0.05, 0.07, 0.03)

  expect_equal(entropy(x), sum(-x / sum(x) * log(x / sum(x), base = 2)))
})

test_that("palma returns the alternative palma inequality measure", {

  x <- c(0.2, 0.3, 0.5)

  expect_equal(palma(x),
               sum(x
                   [cut(x, stats::quantile(x, probs = seq(0, 1, 0.1)),
                        include.lowest = TRUE, labels = FALSE) > 9]) /
                 sum(x
                     [cut(x, stats::quantile(x, probs = seq(0, 1, 0.1)),
                          include.lowest = TRUE, labels = FALSE) <= 4]))
})

test_that("concstats returns a data frame", {

  x <- c(0.4, 0.2, 0.15, 0.1, 0.05, 0.07, 0.03)

  expect_true(is.data.frame(concstats(x)), "data.frame")
})
