local_edition(3)

## inequ

test_that("inequ function operates / switches properly", {

  x <- c(0.2, 0.3, 0.4, 0.1)
  x1 <- c(0.1, 0.2, 0.3, 0.4)
  x2 <- c(0.4, 0.3, 0.2, 0.1)

  expect_equal(inequ(x, type = "entropy"), entropy(x))
  expect_equal(inequ(x, unbiased = TRUE, type = "entropy"),
               entropy(x, unbiased = TRUE))
  expect_equal(inequ(x1, type = "gini"), gini(x1))
  expect_equal(inequ(x1, unbiased = TRUE, type = "gini"),
               gini(x1, unbiased = TRUE))
  expect_equal(inequ(x, type = "simpson"), simpson(x))
  expect_equal(inequ(x, unbiased = TRUE, type = "simpson"),
               simpson(x, unbiased = TRUE))
  expect_equal(inequ(x1, type = "palma"), palma(x1))
  expect_equal(inequ(x2, type = "grs"), grs(x2))

})


## entropy

test_that("entropy function operates properly", {

  x <- c(0.2, 0.3, 0.4, 0.1)
  x1 <- c(0.2, 0.3, 0.4, -0.1)
  x2 <- c(0.2, 0.3, 0.4, 0.1, NA)

  expect_true(any(is.na(x2)), all(!is.na(x2)))
  expect_true(all(round(x) == 0), (abs(x) > 0 & abs(x) <= 1))
  expect_vector(x, ptype = double(), size = 4)
  expect_true(is.numeric(x), label = "numeric values returned")
  expect_equal(entropy(x2, na.rm = FALSE), NA_integer_)
  expect_error(entropy(x1)(!(sum(x1) == 1)),
               label = "vector does not sum to 1 or 100")
})

test_that("entropy returns the entropy measure ", {

  x <- c(0.2, 0.3, 0.4, 0.1)

  expect_equal(entropy(x), sum(-x / sum(x) * log(x / sum(x), base = 2)))
})

test_that("entropy returns the unbiased entropy measure ", {

  x <- c(0.2, 0.3, 0.4, 0.1)

  expect_equal(entropy(x, unbiased = TRUE),
               (sum(-x / sum(x) * log(x / sum(x), base = 2))
                / log(sum(x > 0), base = 2)))
})

## gini

test_that("gini function operates properly", {

  x <- c(0.2, 0.3, 0.4, 0.1)
  x1 <- c(0.2, 0.3, 0.4, -0.1)
  x2 <- c(0.2, 0.3, 0.4, 0.1, NA)
  x3 <- c(0.1, 0.2, 0.3, 0.4)

  expect_true(any(is.na(x2)), all(!is.na(x2)))
  expect_true(all(round(x) == 0), (abs(x) > 0 & abs(x) <= 1))
  expect_vector(x, ptype = double(), size = 4)
  expect_true(is.numeric(x), label = "numeric values returned")
  expect_equal(gini(x2, na.rm = FALSE), NA_integer_)
  expect_equal(sort(x), x3)
  expect_error(gini(x1)(!(sum(x1) == 1)),
               label = "vector does not sum to 1")
})

test_that("gini returns the gini measure", {

  x <- c(0.1, 0.2, 0.3, 0.4)

  expect_equal(gini(x), 2 * sum(x * seq_len(length(x))
                                / length(x) * sum(x)) - 1 - (1 / length(x)))
})

test_that("gini returns the unbiased gini measure", {

  x <- c(0.1, 0.2, 0.3, 0.4)

  expect_equal(gini(x, unbiased = TRUE), length(x) / (length(x) - 1) *
                 (2 * sum(x * seq_len(length(x))
                                / length(x) * sum(x)) - 1 - (1 / length(x))))
})


## simpson

test_that("simpson function operates properly", {

  x <- c(0.2, 0.3, 0.4, 0.1)
  x1 <- c(0.2, 0.3, 0.4, -0.1)
  x2 <- c(0.2, 0.3, 0.4, 0.1, NA)

  expect_true(any(is.na(x2)), all(!is.na(x2)))
  expect_true(all(round(x) == 0), (abs(x) > 0 & abs(x) <= 1))
  expect_vector(x, ptype = double(), size = 4)
  expect_true(is.numeric(x), label = "numeric values returned")
  expect_equal(simpson(x2, na.rm = FALSE), NA_integer_)
  expect_error(simpson(x1)(!(sum(x1) == 1)),
               label = "vector does not sum to 1 or 100")
})

test_that("berry returns the gini measure", {

  x <- c(0.2, 0.3, 0.4, 0.1)

  expect_equal(simpson(x), 1 - sum(x ^ 2))
})

## palma

test_that("palma function operates properly", {

  x <- c(0.2, 0.3, 0.4, 0.1)
  x1 <- c(0.2, 0.3, 0.4, -0.1)
  x2 <- c(0.2, 0.3, 0.4, 0.1, NA)
  x3 <- c(0.1, 0.2, 0.3, 0.4)

  expect_true(any(is.na(x2)), all(!is.na(x2)))
  expect_true(all(round(x) == 0), (abs(x) > 0 & abs(x) <= 1))
  expect_vector(x, ptype = double(), size = 4)
  expect_true(is.numeric(x), label = "numeric values returned")
  expect_equal(palma(x2, na.rm = FALSE), NA_integer_)
  expect_equal(sort(x), x3)
  expect_error(palma(x1)(!(sum(x1) == 1)),
               label = "vector does not sum to 1 or 100")
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

## grs

test_that("grs function operates properly", {

  x <- c(0.2, 0.3, 0.4, 0.1)
  x1 <- c(0.2, 0.3, 0.4, -0.1)
  x2 <- c(0.2, 0.3, 0.4, 0.1, NA)
  x3 <- c(0.4, 0.3, 0.2, 0.1)

  expect_true(any(is.na(x2)), all(!is.na(x2)))
  expect_true(all(round(x) == 0), (abs(x) > 0 & abs(x) <= 1))
  expect_vector(x, ptype = double(), size = 4)
  expect_true(is.numeric(x), label = "numeric values returned")
  expect_equal(grs(x2, na.rm = FALSE), NA_integer_)
  expect_equal(sort(x, decreasing = TRUE), x3)
  expect_error(grs(x1)(!(sum(x1) == 1)),
               label = "vector does not sum to 1 or 100")
})

test_that("grs returns the alternative grs measure", {

  x <- c(0.4, 0.3, 0.2, 0.1)

  expect_equal(grs(x), sum((sum(x > 0) ^ 2 * x[1] + 0.3 * x ^ 2) /
                             (sum(x > 0) ^ 2 + sum(x > 0) * 0.3 * x[1] * x) *
                             x))
})

test_that("all_inequ returns a data frame", {

  x <- c(0.4, 0.2, 0.15, 0.1, 0.05, 0.07, 0.03)

  expect_true(is.data.frame(all_inequ(x)), "data.frame")
})
