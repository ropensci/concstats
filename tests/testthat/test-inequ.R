local_edition(3)

## concstats_inequ

test_that("concstats_inequ function operates / switches properly", {

  x <- c(0.2, 0.3, 0.4, 0.1)
  x1 <- c(0.1, 0.2, 0.3, 0.4)
  x2 <- c(0.4, 0.3, 0.2, 0.1)

  expect_equal(concstats_inequ(x, type = "entropy"), concstats_entropy(x))
  expect_equal(concstats_inequ(x, unbiased = TRUE, type = "entropy"),
               concstats_entropy(x, unbiased = TRUE))
  expect_equal(concstats_inequ(x1, type = "gini"), concstats_gini(x1))
  expect_equal(concstats_inequ(x1, unbiased = TRUE, type = "gini"),
               concstats_gini(x1, unbiased = TRUE))
  expect_equal(concstats_inequ(x, type = "simpson"), concstats_simpson(x))
  expect_equal(concstats_inequ(x, unbiased = TRUE, type = "simpson"),
               concstats_simpson(x, unbiased = TRUE))
  expect_equal(concstats_inequ(x1, type = "palma"), concstats_palma(x1))
  expect_equal(concstats_inequ(x2, type = "grs"), concstats_grs(x2))

})


## concstats_entropy

test_that("concstats_entropy function operates properly", {

  x <- c(0.2, 0.3, 0.4, 0.1)
  x1 <- c(0.2, 0.3, 0.4, -0.1)
  x2 <- c(0.2, 0.3, 0.4, 0.1, NA)

  expect_true(any(is.na(x2)), all(!is.na(x2)))
  expect_true(all(round(x) == 0), (abs(x) > 0 & abs(x) <= 1))
  expect_vector(x, ptype = double(), size = 4)
  expect_true(is.numeric(x), label = "numeric values returned")
  expect_equal(concstats_entropy(x2, na.rm = FALSE), NA_integer_)
  expect_error(concstats_entropy(x1, !isTRUE(all.equal(1, sum(x),
                                          tolerance = .Machine$double.eps^0.25))),
               "vector does not sum to 1")
})

test_that("concstats_entropy returns the entropy measure ", {

  x <- c(0.2, 0.3, 0.4, 0.1)

  expect_equal(concstats_entropy(x), sum(-x / sum(x) * log(x / sum(x), base = 2)))
})

test_that("concstats_entropy returns the unbiased entropy measure ", {

  x <- c(0.2, 0.3, 0.4, 0.1)

  expect_equal(concstats_entropy(x, unbiased = TRUE),
               (sum(-x / sum(x) * log(x / sum(x), base = 2))
                / log(sum(x > 0), base = 2)))
})

## concstats_gini

test_that("concstats_gini function operates properly", {

  x <- c(0.2, 0.3, 0.4, 0.1)
  x1 <- c(0.2, 0.3, 0.4, -0.1)
  x2 <- c(0.2, 0.3, 0.4, 0.1, NA)
  x3 <- c(0.1, 0.2, 0.3, 0.4)

  expect_true(any(is.na(x2)), all(!is.na(x2)))
  expect_true(all(round(x) == 0), (abs(x) > 0 & abs(x) <= 1))
  expect_vector(x, ptype = double(), size = 4)
  expect_true(is.numeric(x), label = "numeric values returned")
  expect_equal(concstats_gini(x2, na.rm = FALSE), NA_integer_)
  expect_equal(sort(x), x3)
  expect_error(concstats_gini(x1, !isTRUE(all.equal(1, sum(x),
                                          tolerance = .Machine$double.eps^0.25))),
               "vector does not sum to 1")
})

test_that("concstats_gini returns the gini measure", {

  x <- c(0.1, 0.2, 0.3, 0.4)

  expect_equal(concstats_gini(x), 2 * sum(x * seq_len(length(x))
                                / length(x) * sum(x)) - 1 - (1 / length(x)))
})

test_that("concstats_gini returns the unbiased gini measure", {

  x <- c(0.1, 0.2, 0.3, 0.4)

  expect_equal(concstats_gini(x, unbiased = TRUE), length(x) / (length(x) - 1) *
                 (2 * sum(x * seq_len(length(x))
                                / length(x) * sum(x)) - 1 - (1 / length(x))))
})


## concstats_simpson

test_that("concstats_simpson function operates properly", {

  x <- c(0.2, 0.3, 0.4, 0.1)
  x1 <- c(0.2, 0.3, 0.4, -0.1)
  x2 <- c(0.2, 0.3, 0.4, 0.1, NA)

  expect_true(any(is.na(x2)), all(!is.na(x2)))
  expect_true(all(round(x) == 0), (abs(x) > 0 & abs(x) <= 1))
  expect_vector(x, ptype = double(), size = 4)
  expect_true(is.numeric(x), label = "numeric values returned")
  expect_equal(concstats_simpson(x2, na.rm = FALSE), NA_integer_)
  expect_error(concstats_simpson(x1, !isTRUE(all.equal(1, sum(x),
                                          tolerance = .Machine$double.eps^0.25))),
               "vector does not sum to 1")
})

test_that("concstats_simpson returns the Simpson measure", {

  x <- c(0.2, 0.3, 0.4, 0.1)

  expect_equal(concstats_simpson(x), 1 - sum(x ^ 2))
})

## concstats_palma

test_that("concstats_palma function operates properly", {

  x <- c(0.2, 0.3, 0.4, 0.1)
  x1 <- c(0.2, 0.3, 0.4, -0.1)
  x2 <- c(0.2, 0.3, 0.4, 0.1, NA)
  x3 <- c(0.1, 0.2, 0.3, 0.4)

  expect_true(any(is.na(x2)), all(!is.na(x2)))
  expect_true(all(round(x) == 0), (abs(x) > 0 & abs(x) <= 1))
  expect_vector(x, ptype = double(), size = 4)
  expect_true(is.numeric(x), label = "numeric values returned")
  expect_equal(concstats_palma(x2, na.rm = FALSE), NA_integer_)
  expect_equal(sort(x), x3)
  expect_error(concstats_palma(x1, !isTRUE(all.equal(1, sum(x),
                                          tolerance = .Machine$double.eps^0.25))),
               "vector does not sum to 1")
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

## concstats_grs

test_that("concstats_grs function operates properly", {

  x <- c(0.2, 0.3, 0.4, 0.1)
  x1 <- c(0.2, 0.3, 0.4, -0.1)
  x2 <- c(0.2, 0.3, 0.4, 0.1, NA)
  x3 <- c(0.4, 0.3, 0.2, 0.1)

  expect_true(any(is.na(x2)), all(!is.na(x2)))
  expect_true(all(round(x) == 0), (abs(x) > 0 & abs(x) <= 1))
  expect_vector(x, ptype = double(), size = 4)
  expect_true(is.numeric(x), label = "numeric values returned")
  expect_equal(concstats_grs(x2, na.rm = FALSE), NA_integer_)
  expect_equal(sort(x, decreasing = TRUE), x3)
  expect_error(concstats_grs(x1, !isTRUE(all.equal(1, sum(x),
                                          tolerance = .Machine$double.eps^0.25))),
               "vector does not sum to 1")
})

test_that("concstats_grs returns the alternative grs measure", {

  x <- c(0.4, 0.3, 0.2, 0.1)

  expect_equal(concstats_grs(x), sum((sum(x > 0) ^ 2 * x[1] + 0.3 * x ^ 2) /
                             (sum(x > 0) ^ 2 + sum(x > 0) * 0.3 * x[1] * x) *
                             x))
})

test_that("concstats_all_inequ returns a data frame", {

  x <- c(0.4, 0.2, 0.15, 0.1, 0.05, 0.07, 0.03)

  expect_true(is.data.frame(concstats_all_inequ(x)), "data.frame")
})
