library(testthat)

local_edition(3)

## mstruct

test_that("mstruct function operates / switches properly", {

  x <- c(0.2, 0.3, 0.4, 0.1)
  x2 <- c(0.4, 0.3, 0.2, 0.1)

  expect_equal(mstruct(x, type = "firm"), firm(x))
  expect_equal(mstruct(x, type = "nrs_eq"), nrs_eq(x))
  expect_equal(mstruct(x2, type = "top"), top(x2))
  expect_equal(mstruct(x2, type = "top3"), top3(x2))
  expect_equal(mstruct(x2, type = "top5"), top5(x2))

})


## firm

test_that("firm function operates properly", {

  x <- c(0.2, 0.3, 0.4, 0.1)
  x1 <- c(0.2, 0.3, 0.4, -0.1)
  x2 <- c(0.2, 0.3, 0.4, 0.1, NA)

  expect_true(any(is.na(x2)), all(!is.na(x2)))
  expect_true(all(round(x) == 0), (abs(x) > 0 & abs(x) <= 1))
  expect_vector(x, ptype = double(), size = 4)
  expect_true(is.numeric(x), label = "numeric values returned")
  expect_equal(firm(x2, na.rm = FALSE), NA_integer_)
  expect_error(firm(x1)(!(sum(x1) == 1)),
               label = "vector does not sum to 1 or 100")
})

test_that("firm returns number of firms ", {

  x <- c(0.2, 0.3, 0.4, 0.1)

  expect_equal(firm(x), sum(x > 0))
})

## nrs_eq

test_that("nrs_eq function operates properly", {

  x <- c(0.2, 0.3, 0.4, 0.1)
  x1 <- c(0.2, 0.3, 0.4, -0.1)
  x2 <- c(0.2, 0.3, 0.4, 0.1, NA)


  expect_true(any(is.na(x2)), all(!is.na(x2)))
  expect_true(all(round(x) == 0), (abs(x) > 0 & abs(x) <= 1))
  expect_vector(x, ptype = double(), size = 4)
  expect_true(is.numeric(x), label = "numeric values returned")
  expect_equal(nrs_eq(x2, na.rm = FALSE), NA_integer_)
  expect_error(nrs_eq(x1)(!(sum(x1) == 1)),
               label = "vector does not sum to 1 or 100")
})

test_that("nrs_eq returns numbers equivalent", {

  x <- c(0.2, 0.3, 0.4, 0.1)

  expect_equal(nrs_eq(x), 1 / sum(x ^ 2))
})

## top

test_that("top function operates properly", {

  x <- c(0.2, 0.3, 0.4, 0.1)
  x1 <- c(0.2, 0.3, 0.4, -0.1)
  x2 <- c(0.2, 0.3, 0.4, 0.1, NA)
  x3 <- c(0.4, 0.3, 0.2, 0.1)

  expect_true(any(is.na(x2)), all(!is.na(x2)))
  expect_true(all(round(x) == 0), (abs(x) > 0 & abs(x) <= 1))
  expect_vector(x, ptype = double(), size = 4)
  expect_true(is.numeric(x), label = "numeric values returned")
  expect_equal(top(x2, na.rm = FALSE), NA_integer_)
  expect_equal(sort(x, decreasing = TRUE), x3)
  expect_error(top(x1)(!(sum(x1) == 1)),
               label = "vector does not sum to 1 or 100")
})

test_that("top returns top market share", {

  x <- c(0.4, 0.3, 0.2, 0.1)

  expect_equal(top(x), x[1] * 100)
})

## top3

test_that("top3 function operates properly", {

  x <- c(0.2, 0.3, 0.4, 0.1)
  x1 <- c(0.2, 0.3, 0.4, -0.1)
  x2 <- c(0.2, 0.3, 0.4, 0.1, NA)
  x3 <- c(0.4, 0.3, 0.2, 0.1)

  expect_true(any(is.na(x2)), all(!is.na(x2)))
  expect_true(all(round(x) == 0), (abs(x) > 0 & abs(x) <= 1))
  expect_vector(x, ptype = double(), size = 4)
  expect_true(is.numeric(x), label = "numeric values returned")
  expect_equal(top3(x2, na.rm = FALSE), NA_integer_)
  expect_equal(sort(x, decreasing = TRUE), x3)
  expect_error(top3(x1)(!(sum(x1) == 1)),
               label = "vector does not sum to 1 or 100")
})

test_that("top3 returns sum of top 3 market shares", {

  x <- c(0.4, 0.3, 0.2, 0.1)

  expect_equal(top3(x), sum(x[1:3], na.rm = TRUE) * 100)
})

## top5

test_that("top5 function operates properly", {

  x <- c(0.2, 0.3, 0.4, 0.1)
  x1 <- c(0.2, 0.3, 0.4, -0.1)
  x2 <- c(0.2, 0.3, 0.4, 0.1, NA)
  x3 <- c(0.4, 0.3, 0.2, 0.1)

  expect_true(any(is.na(x2)), all(!is.na(x2)))
  expect_true(all(round(x) == 0), (abs(x) > 0 & abs(x) <= 1))
  expect_vector(x, ptype = double(), size = 4)
  expect_true(is.numeric(x), label = "numeric values returned")
  expect_equal(top5(x2, na.rm = FALSE), NA_integer_)
  expect_equal(sort(x, decreasing = TRUE), x3)
  expect_error(top5(x1)(!(sum(x1) == 1)),
               label = "vector does not sum to 1 or 100")
})

test_that("top5 returns sum of top 5 market shares", {

  x <- c(0.4, 0.15, 0.15, 0.2, 0.1)
  x1 <- c(0.4, 0.15, 0.15, 0.3)

  expect_equal(top5(x), sum(x[1:5], na.rm = TRUE) * 100)
  expect_equal(top5(x1), sum(x1[1:5], na.rm = TRUE) * 100)
})

test_that("all_mstruct returns a data frame", {

  x <- c(0.4, 0.2, 0.15, 0.1, 0.05, 0.07, 0.03)

  expect_true(is.data.frame(all_mstruct(x)), "data.frame")
})
