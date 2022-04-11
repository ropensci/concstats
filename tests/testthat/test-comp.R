library(testthat)

local_edition(3)

## comp

test_that("comp function operates / switches properly", {

  x <- c(76.83, 0.07, 23.10, 0, NA)
  x1 <- c(0.2, 0.3, 0.4, 0.1)

  expect_equal(comp(x, type = "hhi"), hhi(x))
  expect_equal(comp(x, unbiased = TRUE, type = "hhi"), hhi(x, unbiased = TRUE))
  expect_equal(comp(x1, type = "hhi_d"), hhi_d(x1))
  expect_equal(comp(x, type = "hhi_min"), hhi_min(x))
  expect_equal(comp(x1, type = "dom"), dom(x1))
  expect_equal(comp(x1, type = "sten"), sten(x1))

})


## hhi

test_that("hhi function operates properly", {

  x <- c(76.83, 0.07, 23, 0)
  x1 <- c(0.2, 0.3, 0.4, -0.1)
  x2 <- c(76.83, 0.07, 23.10, 0, NA)

  expect_true(any(is.na(x2)), all(!is.na(x2)))
  expect_vector(1:10, ptype = integer(), size = 10)
  expect_true(is.numeric(x), label = "numeric values returned")
  expect_equal(hhi(x2, na.rm = FALSE), NA_integer_)
  expect_error(hhi(x1)(!(sum(x1) == 1 | sum(x1) == 100)),
               "vector does not sum to 1 or 100")
  expect_error(hhi(x)(!(sum(x) == 1 | sum(x) == 100)),
               "vector does not sum to 1 or 100")
})

test_that("hhi returns sum of squared shares", {

  x <- c(76.83, 0.07, 23.10, 0, 0)

  expect_equal(hhi(x), sum(x^2))
})

test_that("hhi returns sum of squared shares as decimal", {

  x <- c(0.2, 0.3, 0.4, 0.1, 0)

  expect_equal(hhi(x), sum(x^2))
})

test_that("hhi returns unbiased sum of squared shares", {

  x <- c(76.83, 0.07, 23.10, 0, 0)

  expect_equal(hhi(x, unbiased = TRUE),
               (sum(x ^ 2) - (1 / sum(x > 0))) / (1 - (1 / sum(x > 0))))
})

## hhi_min

test_that("hhi_min function operates properly", {

  x <- c(76.83, 0.07, 23, 0)
  x1 <- c(0.2, 0.3, 0.4, -0.1)
  x2 <- c(76.83, 0.07, 23.10, 0, NA)

  expect_true(any(is.na(x2)), all(!is.na(x2)))
  expect_vector(1:10, ptype = integer(), size = 10)
  expect_true(is.numeric(x), label = "numeric values returned")
  expect_equal(hhi(x2, na.rm = FALSE), NA_integer_)
  expect_error(hhi(x1)(!(sum(x1) == 1)),
               label = "vector does not sum to 1")
  expect_error(hhi(x)(!(sum(x) == 100)),
               label = "vector does not sum to 100")
})

test_that("hhi_min returns min of squared shares", {

  x <- c(76.83, 0.07, 23.10, 0, 0)

  expect_equal(hhi_min(x), 1 / sum(x > 0))
})

test_that("hhi_min returns min of squared shares as decimal", {

  x <- c(0.2, 0.3, 0.4, 0.1, 0)

  expect_equal(hhi_min(x), 1 / sum(x > 0))
})

## hhi_d

test_that("hhi_d function operates properly", {

  x <- c(0.2, 0.3, 0.4, 0.1)
  x1 <- c(0.2, 0.3, 0.4, -0.1)
  x2 <- c(0.2, 0.3, 0.4, 0.1, NA)

  expect_true(any(is.na(x2)), all(!is.na(x2)))
  expect_true(all(round(x) == 0), (abs(x) > 0 & abs(x) <= 1))
  expect_vector(x, ptype = double(), size = 4)
  expect_true(is.numeric(x), label = "numeric values returned")
  expect_equal(hhi(x2, na.rm = FALSE), NA_integer_)
  expect_error(hhi(x1)(!(sum(x1) == 1)),
               label = "vector does not sum to 1 or 100")
})

test_that("hhi_d returns dual of hhi", {

  x <- c(0.2, 0.3, 0.4, 0.1, 0)

  expect_equal(hhi_d(x), 1 - 1 / (sum(x > 0) * sum(x ^ 2)))
})

## dom

test_that("dom function operates properly", {

  x <- c(0.2, 0.3, 0.4, 0.1)
  x1 <- c(0.2, 0.3, 0.4, -0.1)
  x2 <- c(0.2, 0.3, 0.4, 0.1, NA)

  expect_true(any(is.na(x2)), all(!is.na(x2)))
  expect_true(all(round(x) == 0), (abs(x) > 0 & abs(x) <= 1))
  expect_vector(x, ptype = double(), size = 4)
  expect_true(is.numeric(x), label = "numeric values returned")
  expect_equal(hhi(x2, na.rm = FALSE), NA_integer_)
  expect_error(hhi(x1)(!(sum(x1) == 1)),
               label = "vector does not sum to 1 or 100")
})

test_that("dom returns dominance index", {

  x <- c(0.2, 0.3, 0.4, 0.1, 0)

  expect_equal(dom(x), sum((x ^ 2 / (sum(x ^ 2))) ^ 2))
})

## sten

test_that("sten function operates properly", {

  x <- c(0.2, 0.3, 0.4, 0.1)
  x1 <- c(0.2, 0.3, 0.4, -0.1)
  x2 <- c(0.4, 0.3, 0.2, 0.1)
  x3 <- c(0.2, 0.3, 0.4, 0.1, NA)

  expect_true(any(is.na(x3)), all(!is.na(x3)))
  expect_equal(sort(x, decreasing = TRUE), x2)
  expect_true(all(round(x) == 0), (abs(x) > 0 & abs(x) <= 1))
  expect_vector(x, ptype = double(), size = 4)
  expect_true(is.numeric(x), label = "numeric values returned")
  expect_equal(hhi(x3, na.rm = FALSE), NA_integer_)
  expect_error(hhi(x1)(!(sum(x1) == 1)),
               label = "vector does not sum to 1 or 100")
})

test_that("sten returns stenbacka index", {

  x <- c(0.4, 0.3, 0.2, 0.1)

  expect_equal(sten(x), 0.5 * (1 - 1 * (x[1] ^ 2 - x[2] ^ 2)) * 100)
})

test_that("all_comp returns a data frame", {

  x <- c(0.4, 0.2, 0.15, 0.1, 0.05, 0.07, 0.03)

  expect_true(is.data.frame(all_comp(x)), "data.frame")
})
