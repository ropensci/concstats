local_edition(3)

## concstats_mstruct

test_that("concstats_mstruct function operates / switches properly", {
#' @srrstats {G5.1} Data used to test, made generally available and run
#' examples.
  x <- c(0.2, 0.3, 0.4, 0.1)
  x1 <- c(0.2, 0.3, 0.5, -0.1)
  x1b <- c()
  x2 <- c(0.4, 0.3, 0.2, 0.1)
  x3 <- c(0.2, 0.3, 0.4, 0.1, NA)
  x8 <- c(-0.2, -0.3, -0.4, -0.100001)
  x9 <- c(NA, NA, NA, NA, NA, NA, NA, NA)
  xch <- c("a", "b", "c", "d", "e", "f", "g", "h")

#' @srrstats {G5.3} Expected to return objects containing no missing (`NA`)
  expect_true(any(is.na(x3)), all(!is.na(x3)))
  expect_vector(x, ptype = numeric(), size = 4)
  expect_equal(concstats_mstruct(x, type = "firm"), concstats_firm(x))
  expect_equal(concstats_mstruct(x, type = "nrs_eq"), concstats_nrs_eq(x))
  expect_equal(concstats_mstruct(x2, type = "top"), concstats_top(x2))
  expect_equal(concstats_mstruct(x2, type = "top3"), concstats_top3(x2))
  expect_equal(concstats_mstruct(x2, type = "top5"), concstats_top5(x2))
  expect_equal(concstats_mstruct(x, type = "Firm"), concstats_firm(x))
  expect_equal(concstats_mstruct(x, type = "all"), concstats_all_mstruct(x))
  expect_error(concstats_mstruct(x1b, na.rm = TRUE))

})

## concstats_firm

test_that("concstats_firm function operates properly", {
#' @srrstats {G5.1} Data used to test, made generally available and run
#'  examples.
  x <- c(0.2, 0.3, 0.4, 0.1)
  x1 <- c(0.2, 0.3, 0.5, -0.1)
  x1b <- c()
  x2 <- c(0.2, 0.3, 0.4, 0.1, NA)
  x4 <- c(20, 30, 40, 10)
  x5 <- c(0.2, 0.3, 0.4, 0.10001)
  x6 <- c(0.2, 0.3, 0.4, 0.1, 0.0000000001)
  x8 <- c(-0.2, -0.3, -0.4, -0.100001)
  x9 <- c(NA, NA, NA, NA, NA, NA, NA, NA)
  xch <- c("a", "b", "c", "d", "e", "f", "g", "h")
  na.rm <- as.logical(TRUE | FALSE)


#' @srrstats {G5.3} Expected to return objects containing no missing (`NA`)
  expect_true(any(is.na(x2)), all(!is.na(x2)))

  expect_true(all(round(x) == 0), (abs(x) > 0 & abs(x) <= 1))
  expect_vector(x, ptype = numeric(), size = 4)
  expect_equal(x, as.numeric(x4 / sum(x4)))
  expect_equal(concstats_firm(x2, na.rm = FALSE), NA_real_)
#' @srrstats {G5.2, G5.2a, G5.2b, G5.8, G5.8a, G5.8b} Edge test for data of
#'  unsupported types
  expect_error(concstats_firm(xch, !isTRUE(is.numeric(xch),
  "x in `concstats_firm` must be a numeric vector\n",
  "You have provided an object of class: ", class(xch)[1])))
#' @srrstats {G5.2, G5.2a, G5.2b, G5.8c} Error on vector with all-`NA` fields
  expect_error(concstats_firm(x9, na.rm = TRUE))
  expect_error(concstats_firm(x8, na.rm = TRUE))
  expect_error(concstats_firm(x1b, na.rm = TRUE))
  expect_warning(concstats_firm(x, na.rm = 0))
#' @srrstats {G3.0, EA6.0, EA6.0e} Testing values of single-valued objects.
  expect_equal(concstats_firm(x, isTRUE(all.equal(1, sum(x),
                                       tolerance = .Machine$double.eps^0.25))),
               concstats_firm(x))
#' @srrstats {G3.0, G5.9, G5.9a} Adding trivial noise
  expect_equal(concstats_firm(x5, isTRUE(all.equal(1, sum(x),
                                       tolerance = .Machine$double.eps^0.25))),
               concstats_firm(x5))
  expect_equal(concstats_firm(x6, isTRUE(all.equal(1, sum(x),
                                       tolerance = .Machine$double.eps^0.25))),
               concstats_firm(x6))
#' @srrstats {G3.0, G5.2, G5.2a, G5.2b, EA6.0, EA6.0e} Return values,
#'  single-valued objects
  expect_error(concstats_firm(x1, !isTRUE(all.equal(1, sum(x1),
                                       tolerance = .Machine$double.eps^0.25))),
               "vector x in `concstats_firm` does not sum to 1")
  expect_length(na.rm, 1L)
  expect_type(na.rm, "logical")

})

  test_that("concstats_firm returns number of firms ", {

#' @srrstats {G5.5, G5.4, G5.4a, G5.4c} Stored values (share_2018_) and vector
#'  are drawn from [this paper](https://doi.org/10.1515/zfgg-2022-0002),
#'   page 26/27

  x <- c(0.2, 0.3, 0.4, 0.1)
  share_2018_firm <- 22
  share_2018 <- c(0.012663407, 0.029367501, 0.014456455, 0.012046011,
                  0.007477799, 0.189784408, 0.008738591, 0.015635544,
                  0.012787201, 0.013071539, 0.046268385, 0.006580823, 0.009102,
                  0.00760554, 0.047173998, 0.034356881, 0.137813902,
                  0.016876624, 0.065780114, 0.053775553, 0.228519883,
                  0.030117841)

#' @srrstats {G3.0, EA6.0, EA6.0e} Return values, single-valued objects.
  expect_equal(concstats_firm(share_2018), share_2018_firm,
               tolerance = .Machine$double.eps^0.25)
  expect_equal(concstats_firm(x), sum(x > 0))
#' @srrstats {EA6.0, EA6.0a} Return values
  expect_true(is.numeric(share_2018_firm), label = "numeric values returned")
  })

## concstats_nrs_eq

test_that("concstats_nrs_eq function operates properly", {
#' @srrstats {G5.1} Data used to test, made generally available and run
#'  examples.
  x <- c(0.2, 0.3, 0.4, 0.1)
  x1 <- c(0.2, 0.3, 0.5, -0.1)
  x1b <- c()
  x2 <- c(0.2, 0.3, 0.4, 0.1, NA)
  x4 <- c(20, 30, 40, 10)
  x5 <- c(0.2, 0.3, 0.4, 0.10001)
  x6 <- c(0.2, 0.3, 0.4, 0.1, 0.0000000001)
  x8 <- c(-0.2, -0.3, -0.4, -0.100001)
  x9 <- c(NA, NA, NA, NA, NA, NA, NA, NA)
  xch <- c("a", "b", "c", "d", "e", "f", "g", "h")
  na.rm <- as.logical(TRUE | FALSE)

  expect_true(any(is.na(x2)), all(!is.na(x2)))
  expect_true(all(round(x) == 0), (abs(x) > 0 & abs(x) <= 1))
  expect_vector(x, ptype = numeric(), size = 4)
  expect_equal(x, as.numeric(x4 / sum(x4)))
  expect_equal(concstats_nrs_eq(x2, na.rm = FALSE), NA_real_)
#' @srrstats {G5.2, G5.2a, G5.2b, G5.8, G5.8b} Edge test for data of
#'  unsupported types
  expect_error(concstats_nrs_eq(xch, !isTRUE(is.numeric(xch),
  "x in `concstats_nrs_eq` must be a numeric vector\n",
  "You have provided an object of class: ", class(xch)[1])))
#' @srrstats {G5.2, G5.2a, G5.2b, G5.8c} Error on vector with all-`NA` fields
  expect_error(concstats_nrs_eq(x9, na.rm = TRUE))
  expect_error(concstats_nrs_eq(x8, na.rm = TRUE))
  expect_error(concstats_nrs_eq(x1b, na.rm = TRUE))
  expect_warning(concstats_nrs_eq(x, na.rm = 0))
  #' @srrstats {G3.0, EA6.0, EA6.0e} Testing values of single-valued objects.
  expect_equal(concstats_nrs_eq(x, isTRUE(all.equal(1, sum(x),
                                       tolerance = .Machine$double.eps^0.25))),
               concstats_nrs_eq(x))
#' @srrstats {G3.0, G5.9, G5.9a} *Adding trivial noise
  expect_equal(concstats_nrs_eq(x5, isTRUE(all.equal(1, sum(x),
                                       tolerance = .Machine$double.eps^0.25))),
               concstats_nrs_eq(x5))
  expect_equal(concstats_nrs_eq(x6, isTRUE(all.equal(1, sum(x),
                                       tolerance = .Machine$double.eps^0.25))),
               concstats_nrs_eq(x6))
#' @srrstats {G5.2, G5.2a, G5.2b, EA6.0, EA6.0e} Return values, single-valued
#'  objects
  expect_error(concstats_nrs_eq(x1, !isTRUE(all.equal(1, sum(x1),
                                       tolerance = .Machine$double.eps^0.25))),
               "vector x in `concstats_nrs_eq` does not sum to 1")
  expect_length(na.rm, 1L)
  expect_type(na.rm, "logical")

})

test_that("concstats_nrs_eq returns numbers equivalent", {

  x <- c(0.2, 0.3, 0.4, 0.1)
  share_2018_nrs <- 8.099694
  share_2018 <- c(0.012663407, 0.029367501, 0.014456455, 0.012046011,
                  0.007477799, 0.189784408, 0.008738591, 0.015635544,
                  0.012787201, 0.013071539, 0.046268385, 0.006580823, 0.009102,
                  0.00760554, 0.047173998, 0.034356881, 0.137813902,
                  0.016876624, 0.065780114, 0.053775553, 0.228519883,
                  0.030117841)

#' @srrstats {G3.0, EA6.0, EA6.0e} Return values, single-valued objects.
  expect_equal(concstats_nrs_eq(share_2018), share_2018_nrs,
               tolerance = .Machine$double.eps^0.25)
  expect_equal(concstats_nrs_eq(x), 1 / sum(x ^ 2))
#' @srrstats {EA6.0, EA6.0a} Return values
  expect_true(is.numeric(share_2018_nrs), label = "numeric values returned")
})

## concstats_top

test_that("concstats_top function operates properly", {
#' @srrstats {G5.1} Data used to test, made generally available and run
#'  examples.
  x <- c(0.2, 0.3, 0.4, 0.1)
  x1 <- c(0.2, 0.3, 0.5, -0.1)
  x1b <- c()
  x2 <- c(0.2, 0.3, 0.4, 0.1, NA)
  x3 <- c(0.4, 0.3, 0.2, 0.1)
  x4 <- c(20, 30, 40, 10)
  x5 <- c(0.2, 0.3, 0.4, 0.10001)
  x6 <- c(0.2, 0.3, 0.4, 0.1, 0.0000000001)
  x8 <- c(-0.2, -0.3, -0.4, -0.100001)
  x9 <- c(NA, NA, NA, NA, NA, NA, NA, NA)
  xch <- c("a", "b", "c", "d", "e", "f", "g", "h")
  na.rm <- as.logical(TRUE | FALSE)

  expect_true(any(is.na(x2)), all(!is.na(x2)))
  expect_true(all(round(x) == 0), (abs(x) > 0 & abs(x) <= 1))
  expect_vector(x, ptype = numeric(), size = 4)
  expect_equal(x, as.numeric(x4 / sum(x4)))
  expect_equal(concstats_top(x2, na.rm = FALSE), NA_real_)
  expect_equal(sort(x, decreasing = TRUE), x3)
#' @srrstats {G5.2, G5.2a, G5.2b, G5.8, G5.8b} Edge test for data of
#'  unsupported types
  expect_error(concstats_top(xch, !isTRUE(is.numeric(xch),
  "x in `concstats_top`must be a numeric vector\n",
  "You have provided an object of class: ", class(xch)[1])))
#' @srrstats {G5.2, G5.2a, G5.2b, G5.8c} Error on vector with all-`NA` fields
  expect_error(concstats_top(x9, na.rm = TRUE))
  expect_error(concstats_top(x8, na.rm = TRUE))
  expect_error(concstats_top(x1b, na.rm = TRUE))
  expect_warning(concstats_top(x, na.rm = 0))

#' @srrstats {G3.0, EA6.0, EA6.0e} Testing values of single-valued objects.
  expect_equal(concstats_top(x, isTRUE(all.equal(1, sum(x),
                                       tolerance = .Machine$double.eps^0.25))),
               concstats_top(x))
#' @srrstats {G3.0, G5.9, G5.9a} Adding trivial noise
  expect_equal(concstats_top(x5, isTRUE(all.equal(1, sum(x),
                                       tolerance = .Machine$double.eps^0.25))),
               concstats_top(x5))
  expect_equal(concstats_top(x6, isTRUE(all.equal(1, sum(x),
                                       tolerance = .Machine$double.eps^0.25))),
               concstats_top(x6))
#' @srrstats {G5.2, G5.2a, G5.2b, EA6.0, EA6.0e} Return values, single-valued
#'  objects
  expect_error(concstats_top(x1, !isTRUE(all.equal(1, sum(x1),
                                      tolerance = .Machine$double.eps^0.25))),
               "vector x in `concstats_top` does not sum to 1")
  expect_length(na.rm, 1L)
  expect_type(na.rm, "logical")

})

test_that("concstats_top returns top market share", {

  x <- c(0.4, 0.3, 0.2, 0.1)
  share_2018_top <- 22.85199
  share_2018 <- c(0.012663407, 0.029367501, 0.014456455, 0.012046011,
                  0.007477799, 0.189784408, 0.008738591, 0.015635544,
                  0.012787201, 0.013071539, 0.046268385, 0.006580823, 0.009102,
                  0.00760554, 0.047173998, 0.034356881, 0.137813902,
                  0.016876624, 0.065780114, 0.053775553, 0.228519883,
                  0.030117841)

#' @srrstats {G3.0, EA6.0, EA6.0e} Return values, single-valued objects.
  expect_equal(concstats_top(share_2018), share_2018_top,
               tolerance = .Machine$double.eps^0.25)
  expect_equal(concstats_top(x), x[1] * 100)
#' @srrstats {EA6.0, EA6.0a} Return values
  expect_true(is.numeric(share_2018_top), label = "numeric values returned")
})

## concstats_top3

test_that("concstats_top3 function operates properly", {
#' @srrstats {G5.1} Data used to test, made generally available and run
#'  examples.
  x <- c(0.2, 0.3, 0.4, 0.1)
  x1 <- c(0.2, 0.3, 0.5, -0.1)
  x1b <- c()
  x2 <- c(0.2, 0.3, 0.4, 0.1, NA)
  x3 <- c(0.4, 0.3, 0.2, 0.1)
  x4 <- c(20, 30, 40, 10)
  x5 <- c(0.2, 0.3, 0.4, 0.10001)
  x6 <- c(0.2, 0.3, 0.4, 0.1, 0.0000000001)
  x8 <- c(-0.2, -0.3, -0.4, -0.100001)
  x9 <- c(NA, NA, NA, NA, NA, NA, NA, NA)
  xch <- c("a", "b", "c", "d", "e", "f", "g", "h")
  na.rm <- as.logical(TRUE | FALSE)

  expect_true(any(is.na(x2)), all(!is.na(x2)))
  expect_true(all(round(x) == 0), (abs(x) > 0 & abs(x) <= 1))
  expect_vector(x, ptype = numeric(), size = 4)
  expect_equal(x, as.numeric(x4 / sum(x4)))
  expect_equal(concstats_top3(x2, na.rm = FALSE), NA_real_)
  expect_equal(sort(x, decreasing = TRUE), x3)
#' @srrstats {G5.2, G5.2a, G5.2b, G5.8, G5.8b} Edge test for data of
#'  unsupported types
  expect_error(concstats_top3(xch, !isTRUE(is.numeric(xch),
  "x in `concstats_top3` must be a numeric vector\n",
  "You have provided an object of class: ", class(xch)[1])))
#' @srrstats {G5.2, G5.2a, G5.2b, G5.8c} Error on vector with all-`NA` fields
  expect_error(concstats_top3(x9, na.rm = TRUE))
  expect_error(concstats_top3(x8, na.rm = TRUE))
  expect_error(concstats_top3(x1b, na.rm = TRUE))
  expect_warning(concstats_top3(x, na.rm = 0))
  #' @srrstats {G3.0, EA6.0, EA6.0e} Testing values of single-valued objects.
  expect_equal(concstats_top3(x, isTRUE(all.equal(1, sum(x),
                                       tolerance = .Machine$double.eps^0.25))),
               concstats_top3(x))
#' @srrstats {G3.0, G5.9, G5.9a} *Adding trivial noise
  expect_equal(concstats_top3(x5, isTRUE(all.equal(1, sum(x),
                                       tolerance = .Machine$double.eps^0.25))),
               concstats_top3(x5))
  expect_equal(concstats_top3(x6, isTRUE(all.equal(1, sum(x),
                                       tolerance = .Machine$double.eps^0.25))),
               concstats_top3(x6))
#' @srrstats {G5.2, G5.2a, G5.2b, EA6.0, EA6.0e} Return values, single-valued
#'  objects
  expect_error(concstats_top3(x1, !isTRUE(all.equal(1, sum(x1),
                                      tolerance = .Machine$double.eps^0.25))),
               "vector x in `concstats_top3` does not sum to 1")
  expect_length(na.rm, 1L)
  expect_type(na.rm, "logical")
})

test_that("concstats_top3 returns sum of top 3 market shares", {

  x <- c(0.4, 0.3, 0.2, 0.1)
  share_2018_top3 <- 55.61182
  share_2018 <- c(0.012663407, 0.029367501, 0.014456455, 0.012046011,
                  0.007477799, 0.189784408, 0.008738591, 0.015635544,
                  0.012787201, 0.013071539, 0.046268385, 0.006580823, 0.009102,
                  0.00760554, 0.047173998, 0.034356881, 0.137813902,
                  0.016876624, 0.065780114, 0.053775553, 0.228519883,
                  0.030117841)

#' @srrstats {G3.0, EA6.0, EA6.0e} Return values, single-valued objects.
  expect_equal(concstats_top3(share_2018), share_2018_top3,
               tolerance = .Machine$double.eps^0.25)
  expect_equal(concstats_top3(x), sum(x[1:3], na.rm = TRUE) * 100)
#' @srrstats {EA6.0, EA6.0a} Return values
  expect_true(is.numeric(share_2018_top3), label = "numeric values returned")
})

## concstats_top5

test_that("concstats_top5 function operates properly", {
#' @srrstats {G5.1} Data used to test, made generally available and run
#'  examples.
  x <- c(0.2, 0.3, 0.4, 0.1)
  x1 <- c(0.2, 0.3, 0.5, -0.1)
  x1b <- c()
  x2 <- c(0.2, 0.3, 0.4, 0.1, NA)
  x3 <- c(0.4, 0.3, 0.2, 0.1)
  x4 <- c(20, 30, 40, 10)
  x5 <- c(0.2, 0.3, 0.4, 0.10001)
  x6 <- c(0.2, 0.3, 0.4, 0.1, 0.0000000001)
  x8 <- c(-0.2, -0.3, -0.4, -0.100001)
  x9 <- c(NA, NA, NA, NA, NA, NA, NA, NA)
  xch <- c("a", "b", "c", "d", "e", "f", "g", "h")
  na.rm <- as.logical(TRUE | FALSE)

  expect_true(any(is.na(x2)), all(!is.na(x2)))
  expect_true(all(round(x) == 0), (abs(x) > 0 & abs(x) <= 1))
  expect_vector(x, ptype = numeric(), size = 4)
  expect_equal(x, as.numeric(x4 / sum(x4)))
  expect_equal(concstats_top5(x2, na.rm = FALSE), NA_real_)
  expect_equal(sort(x, decreasing = TRUE), x3)
#' @srrstats {G5.2, G5.2a, G5.2b, G5.8, G5.8b} Edge test for data of
#'  unsupported types
  expect_error(concstats_top5(xch, !isTRUE(is.numeric(xch),
  "x in `constats_top5` must be a numeric vector\n",
  "You have provided an object of class: ", class(xch)[1])))
#' @srrstats {G5.2, G5.2a, G5.2b, G5.8c} Error on vector with all-`NA` fields
  expect_error(concstats_top5(x9, na.rm = TRUE))
  expect_error(concstats_top5(x8, na.rm = TRUE))
  expect_error(concstats_top5(x1b, na.rm = TRUE))
  expect_warning(concstats_top5(x, na.rm = 0))
#' @srrstats {G3.0, EA6.0, EA6.0e} Testing values of single-valued objects.
  expect_equal(concstats_top5(x, isTRUE(all.equal(1, sum(x),
                                       tolerance = .Machine$double.eps^0.25))),
               concstats_top5(x))
#' @srrstats {G3.0, G5.9, G5.9a} Adding trivial noise
  expect_equal(concstats_top5(x5, isTRUE(all.equal(1, sum(x),
                                       tolerance = .Machine$double.eps^0.25))),
               concstats_top5(x5))
  expect_equal(concstats_top5(x6, isTRUE(all.equal(1, sum(x),
                                       tolerance = .Machine$double.eps^0.25))),
               concstats_top5(x6))
#' @srrstats {G5.2, G5.2a, G5.2b, EA6.0, EA6.0e} Return values, single-valued
#'  objects
  expect_error(concstats_top5(x1, !isTRUE(all.equal(1, sum(x1),
                                       tolerance = .Machine$double.eps^0.25))),
               "vector x in `concstats_top5` does not sum to 1")
  expect_length(na.rm, 1L)
  expect_type(na.rm, "logical")

})

test_that("concstats_top5 returns sum of top 5 market shares", {

  x <- c(0.4, 0.15, 0.15, 0.2, 0.1)
  x1 <- c(0.4, 0.15, 0.15, 0.3)
  share_2018_top5 <- 67.56739
  share_2018 <- c(0.012663407, 0.029367501, 0.014456455, 0.012046011,
                  0.007477799, 0.189784408, 0.008738591, 0.015635544,
                  0.012787201, 0.013071539, 0.046268385, 0.006580823, 0.009102,
                  0.00760554, 0.047173998, 0.034356881, 0.137813902,
                  0.016876624, 0.065780114, 0.053775553, 0.228519883,
                  0.030117841)

#' @srrstats {G3.0, EA6.0, EA6.0e} Return values, single-valued objects.
  expect_equal(concstats_top5(share_2018), share_2018_top5,
               tolerance = .Machine$double.eps^0.25)
  expect_equal(concstats_top5(x), sum(x[1:5], na.rm = TRUE) * 100)
  expect_equal(concstats_top5(x1), sum(x1[1:5], na.rm = TRUE) * 100)
#' @srrstats {EA6.0, EA6.0a} Return values
  expect_true(is.numeric(share_2018_top5), label = "numeric values returned")
})

test_that("concstats_all_mstruct returns a data frame", {
#' @srrstats {G5.1} Data used to test, made generally available and run
#'  examples.
  x <- c(0.2, 0.3, 0.4, 0.1)
  x1 <- c(0.2, 0.3, 0.5, -0.1)
  x1b <- c()
  x2 <- c(0.2, 0.3, 0.4, 0.1, NA)
  x4 <- c(20, 30, 40, 10)
  x5 <- c(0.2, 0.3, 0.4, 0.10001)
  x6 <- c(0.2, 0.3, 0.4, 0.1, 0.0000000001)
  x8 <- c(-0.2, -0.3, -0.4, -0.100001)
  x9 <- c(NA, NA, NA, NA, NA, NA, NA, NA)
  xch <- c("a", "b", "c", "d", "e", "f", "g", "h")
  na.rm <- as.logical(TRUE | FALSE)
  dummy_df <- data.frame(Measure = rep(letters[1:5]), Value = c(1, 2, 3, 4, 5))

  expect_vector(x, ptype = numeric(), size = 4)
  expect_true(any(is.na(x2)), all(!is.na(x2)))
#' @srrstats {EA6.0, EA6.0a, EA6.0b, EA6.0c, EA6.0d} Classes, dimensions, and
#'  types of objects
  expect_equal(ncol(dummy_df), 2)
  expect_equal(x, as.numeric(x4 / sum(x4)))
  expect_true(is.numeric(dummy_df$Value))
  expect_type(dummy_df$Value, "double")
  expect_type(dummy_df$Measure, "character")
  expect_identical(names(dummy_df), c("Measure", "Value"))
  expect_true(is.data.frame(concstats_all_mstruct(x)), "data.frame")
  expect_equal(concstats_all_mstruct(x2, na.rm = FALSE), NA_real_)
#' @srrstats {G5.2, G5.2a, G5.2b, G5.8, G5.8b} Edge test for data of
#'  unsupported types
  expect_error(concstats_all_mstruct(xch, !isTRUE(is.numeric(xch),
  "x in `concstats_all_mstruct`must be a numeric vector\n",
  "You have provided an object of class:", class(xch)[1])))
#' @srrstats {G5.2, G5.2a, G5.2b, G5.8c} Error on vector with all-`NA` fields
  expect_error(concstats_all_mstruct(x9, na.rm = TRUE))
  expect_error(concstats_all_mstruct(x8, na.rm = TRUE))
  expect_error(concstats_all_mstruct(x1b, na.rm = TRUE))
  expect_warning(concstats_all_mstruct(x, na.rm = 0))
#' @srrstats {G5.2, G5.2a, G5.2b, EA6.0, EA6.0e} Return values, single-valued
  #'  objects
  expect_error(concstats_all_mstruct(x1, !isTRUE(all.equal(1, sum(x1),
                                       tolerance = .Machine$double.eps^0.25))),
               "vector x in `concstats_all_mstruct` does not sum to 1")
#' @srrstats {G3.0, G5.9, G5.9a} Adding trivial noise
  expect_equal(concstats_all_mstruct(x5, isTRUE(all.equal(1, sum(x),
                                       tolerance = .Machine$double.eps^0.25))),
               concstats_all_mstruct(x5))
  expect_equal(concstats_all_mstruct(x6, isTRUE(all.equal(1, sum(x),
                                       tolerance = .Machine$double.eps^0.25))),
               concstats_all_mstruct(x6))
  expect_length(na.rm, 1L)
  expect_type(na.rm, "logical")
})
