local_edition(3)

## concstats_mstruct

test_that("concstats_mstruct function operates / switches properly", {

  x <- c(0.2, 0.3, 0.4, 0.1)
  x2 <- c(0.4, 0.3, 0.2, 0.1)

  expect_equal(concstats_mstruct(x, type = "firm"), concstats_firm(x))
  expect_equal(concstats_mstruct(x, type = "nrs_eq"), concstats_nrs_eq(x))
  expect_equal(concstats_mstruct(x2, type = "top"), concstats_top(x2))
  expect_equal(concstats_mstruct(x2, type = "top3"), concstats_top3(x2))
  expect_equal(concstats_mstruct(x2, type = "top5"), concstats_top5(x2))

})


## concstats_firm

test_that("concstats_firm function operates properly", {

  x <- c(0.2, 0.3, 0.4, 0.1)
  x1 <- c(0.2, 0.3, 0.4, -0.1)
  x2 <- c(0.2, 0.3, 0.4, 0.1, NA)

  expect_true(any(is.na(x2)), all(!is.na(x2)))
  expect_true(all(round(x) == 0), (abs(x) > 0 & abs(x) <= 1))
  expect_vector(x, ptype = double(), size = 4)
  expect_true(is.numeric(x), label = "numeric values returned")
  expect_equal(concstats_firm(x2, na.rm = FALSE), NA_integer_)
  expect_error(concstats_firm(x1, !isTRUE(all.equal(1, sum(x),
                                        tolerance = .Machine$double.eps^0.25))),
               "vector does not sum to 1")
})

  test_that("concstats_firm returns number of firms ", {

  x <- c(0.2, 0.3, 0.4, 0.1)

  expect_equal(concstats_firm(x), sum(x > 0))
})

## concstats_nrs_eq

test_that("concstats_nrs_eq function operates properly", {

  x <- c(0.2, 0.3, 0.4, 0.1)
  x1 <- c(0.2, 0.3, 0.4, -0.1)
  x2 <- c(0.2, 0.3, 0.4, 0.1, NA)


  expect_true(any(is.na(x2)), all(!is.na(x2)))
  expect_true(all(round(x) == 0), (abs(x) > 0 & abs(x) <= 1))
  expect_vector(x, ptype = double(), size = 4)
  expect_true(is.numeric(x), label = "numeric values returned")
  expect_equal(concstats_nrs_eq(x2, na.rm = FALSE), NA_integer_)
  expect_error(concstats_firm(x1, !isTRUE(all.equal(1, sum(x),
                                          tolerance = .Machine$double.eps^0.25))),
               "vector does not sum to 1")
})

test_that("concstats_nrs_eq returns numbers equivalent", {

  x <- c(0.2, 0.3, 0.4, 0.1)

  expect_equal(concstats_nrs_eq(x), 1 / sum(x ^ 2))
})

## concstats_top

test_that("concstats_top function operates properly", {

  x <- c(0.2, 0.3, 0.4, 0.1)
  x1 <- c(0.2, 0.3, 0.4, -0.1)
  x2 <- c(0.2, 0.3, 0.4, 0.1, NA)
  x3 <- c(0.4, 0.3, 0.2, 0.1)

  expect_true(any(is.na(x2)), all(!is.na(x2)))
  expect_true(all(round(x) == 0), (abs(x) > 0 & abs(x) <= 1))
  expect_vector(x, ptype = double(), size = 4)
  expect_true(is.numeric(x), label = "numeric values returned")
  expect_equal(concstats_top(x2, na.rm = FALSE), NA_integer_)
  expect_equal(sort(x, decreasing = TRUE), x3)
  expect_error(concstats_top(x1, !isTRUE(all.equal(1, sum(x),
                                          tolerance = .Machine$double.eps^0.25))),
               "vector does not sum to 1")
})

test_that("concstats_top returns top market share", {

  x <- c(0.4, 0.3, 0.2, 0.1)

  expect_equal(concstats_top(x), x[1] * 100)
})

## concstats_top3

test_that("concstats_top3 function operates properly", {

  x <- c(0.2, 0.3, 0.4, 0.1)
  x1 <- c(0.2, 0.3, 0.4, -0.1)
  x2 <- c(0.2, 0.3, 0.4, 0.1, NA)
  x3 <- c(0.4, 0.3, 0.2, 0.1)

  expect_true(any(is.na(x2)), all(!is.na(x2)))
  expect_true(all(round(x) == 0), (abs(x) > 0 & abs(x) <= 1))
  expect_vector(x, ptype = double(), size = 4)
  expect_true(is.numeric(x), label = "numeric values returned")
  expect_equal(concstats_top3(x2, na.rm = FALSE), NA_integer_)
  expect_equal(sort(x, decreasing = TRUE), x3)
  expect_error(concstats_top3(x1, !isTRUE(all.equal(1, sum(x),
                                          tolerance = .Machine$double.eps^0.25))),
               "vector does not sum to 1")
})

test_that("concstats_top3 returns sum of top 3 market shares", {

  x <- c(0.4, 0.3, 0.2, 0.1)

  expect_equal(concstats_top3(x), sum(x[1:3], na.rm = TRUE) * 100)
})

## concstats_top5

test_that("concstats_top5 function operates properly", {

  x <- c(0.2, 0.3, 0.4, 0.1)
  x1 <- c(0.2, 0.3, 0.4, -0.1)
  x2 <- c(0.2, 0.3, 0.4, 0.1, NA)
  x3 <- c(0.4, 0.3, 0.2, 0.1)

  expect_true(any(is.na(x2)), all(!is.na(x2)))
  expect_true(all(round(x) == 0), (abs(x) > 0 & abs(x) <= 1))
  expect_vector(x, ptype = double(), size = 4)
  expect_true(is.numeric(x), label = "numeric values returned")
  expect_equal(concstats_top5(x2, na.rm = FALSE), NA_integer_)
  expect_equal(sort(x, decreasing = TRUE), x3)
  expect_error(concstats_top5(x1, !isTRUE(all.equal(1, sum(x),
                                          tolerance = .Machine$double.eps^0.25))),
               "vector does not sum to 1")
})

test_that("concstats_top5 returns sum of top 5 market shares", {

  x <- c(0.4, 0.15, 0.15, 0.2, 0.1)
  x1 <- c(0.4, 0.15, 0.15, 0.3)

  expect_equal(concstats_top5(x), sum(x[1:5], na.rm = TRUE) * 100)
  expect_equal(concstats_top5(x1), sum(x1[1:5], na.rm = TRUE) * 100)
})

test_that("concstats_all_mstruct returns a data frame", {

  x <- c(0.4, 0.2, 0.15, 0.1, 0.05, 0.07, 0.03)

  expect_true(is.data.frame(concstats_all_mstruct(x)), "data.frame")
})
