local_edition(3)

## concstats_comp

test_that("concstats_comp function operates / switches properly", {
#' @srrstats {G5.1} Data used to test, made generally available and run
#' examples.
  x <- c(0.2, 0.3, 0.4, 0.1)
  x1 <- c(0.2, 0.3, 0.4)
  x1b <- c()
  x2 <- c(0.2, 0.3, 0.4, 0.1, NA)
  x4 <- c(0.2, 0.3, 0.4, -0.1)
  x8 <- c(-0.2, -0.3, -0.4, -0.100001)
  x9 <- c(NA, NA, NA, NA, NA, NA, NA, NA)
  xch <- c("a", "b", "c", "d", "e", "f", "g", "h")

#' @srrstats {G5.3} Expected to return objects containing no missing (`NA`)
  expect_true(any(is.na(x2)), all(!is.na(x2)))

  expect_vector(x, ptype = numeric(), size = 4)
  expect_equal(concstats_comp(x, type = "hhi"),
               concstats_hhi(x))
  expect_equal(concstats_comp(x, normalized = TRUE, type = "hhi"),
               concstats_hhi(x, normalized = TRUE))
  expect_equal(concstats_comp(x, type = "hhi_d"), concstats_hhi_d(x))
  expect_equal(concstats_comp(x, type = "hhi_min"), concstats_hhi_min(x))
  expect_equal(concstats_comp(x, type = "dom"), concstats_dom(x))
  expect_equal(concstats_comp(x, type = "sten"), concstats_sten(x))
  expect_equal(concstats_comp(x, type = "HHI"), concstats_hhi(x))
  expect_equal(concstats_comp(x, type = "ALL"), concstats_all_comp(x))
  expect_error(concstats_comp(x1b, na.rm = TRUE))

})

## concstats_hhi

test_that("concstats_hhi function operates properly", {
#' @srrstats {G5.1} Data used to test, made generally available and run
#'  examples.
  x <- c(0.2, 0.3, 0.4, 0.1)
  x1 <- c(0.2, 0.3, 0.4, -0.1)
  x1b <- c()
  x2 <- c(0.2, 0.3, 0.4, 0.1, NA)
  x3 <- c(0.2, 0.4, 0.15, 0.05, 0.22, 0.9)
  x4 <- c(20, 30, 40, 10)
  x5 <- c(0.2, 0.3, 0.4, 0.10001)
  x8 <- c(-0.2, -0.3, -0.4, -0.100001)
  x9 <- c(NA, NA, NA, NA, NA, NA, NA, NA)
  xch <- c("a", "b", "c", "d", "e", "f", "g", "h")

#' @srrstats {G5.3} Expected to return objects containing no missing (`NA`)
  expect_true(any(is.na(x2)), all(!is.na(x2)))

  expect_true(all(round(x) == 0), (abs(x) > 0 & abs(x) <= 1))
  expect_vector(x, ptype = double(), size = 4)
  expect_equal(concstats_hhi(x2, na.rm = FALSE), NA_real_)
#' @srrstats {G5.2, G5.2a, G5.2b, G5.8, G5.8a, G5.8b} Edge test for data of
#'  unsupported types
  expect_error(concstats_hhi(xch, !is.numeric(xch)))
#' @srrstats {G 5.0, G5.2, G5.2a, G5.2b, G5.8c} Error and error messages on
#'  vector with all-`NA` fields
  expect_error(concstats_hhi(x9, na.rm = TRUE))
  expect_error(concstats_hhi(x8, na.rm = TRUE))
  expect_error(concstats_hhi(x1b, na.rm = TRUE))
  expect_error(concstats_hhi(x, na.rm = 0))
  expect_error(concstats_hhi(x, normalized = 0))
#' @srrstats {G3.0, EA6.0, EA6.0e} Return values single-valued objects.
  act <- concstats_hhi(x)
  exp <- concstats_hhi(x4 / sum(x4))
  expect_equal(act, exp, tolerance = .Machine$double.eps^0.25)
#' @srrstats {G3.0, G5.9, G5.9a} Adding trivial noise
  act <- concstats_hhi(x)
  exp <- concstats_hhi(x5)
  expect_equal(act, exp, tolerance = .Machine$double.eps^0.25)
#' @srrstats {G5.2, G5.2a, G5.2b, EA6.0, EA6.0e} Return values, single-valued
#'  objects
  expect_error(concstats_hhi(sum(x1), 1, tolerance = .Machine$double.eps^0.25))

})

test_that("concstats_hhi returns sum of squared shares as decimal", {

#' @srrstats {G5.5, G5.4, G5.4a, G5.4c} Stored values (share_2018_) and vector
#'  are drawn from [this paper](https://doi.org/10.1515/zfgg-2022-0002),
#'   page 26/27
  x <- c(0.2, 0.3, 0.4, 0.1)
  share_2018_hhi <- 0.1234614
  share_2018 <- c(0.012663407, 0.029367501, 0.014456455, 0.012046011,
                  0.007477799, 0.189784408, 0.008738591, 0.015635544,
                  0.012787201, 0.013071539, 0.046268385, 0.006580823, 0.009102,
                  0.00760554, 0.047173998, 0.034356881, 0.137813902,
                  0.016876624, 0.065780114, 0.053775553, 0.228519883,
                  0.030117841)
#' @srrstats {G3.0, EA6.0, EA6.0e} Return values, single-valued objects.
  act <- concstats_hhi(share_2018)
  exp <- share_2018_hhi
  expect_equal(act, exp, tolerance = .Machine$double.eps^0.25)

  expect_equal(concstats_hhi(share_2018), share_2018_hhi,
                                          tolerance = .Machine$double.eps^0.25)
  expect_equal(concstats_hhi(x), sum(x^2))
#' @srrstats {EA6.0, EA6.0a} Return values
  expect_true(is.numeric(share_2018_hhi), label = "numeric values returned")

#' @srrstats {G5.4, G5.4b} Correctness test from hhi package can be checked
#'  just visually in the plot
#'   (https://joss.theoj.org/papers/10.21105/joss.00828)

  us_2012_hhi <- 0.1364397
  us_2012 <- c(1.2, 12.3, 11.5, 9.7, 4.5, 4.2, 0.6, 22.4, 17.9, 7.3, 8.3)
  expect_equal(concstats_hhi(us_2012), us_2012_hhi,
               tolerance = .Machine$double.eps^0.25)
})

test_that("concstats_hhi returns normalized hhi", {

  x <- c(0.2, 0.3, 0.4, 0.1)
  share_2018_hhi2 <- 0.08172152
  share_2018 <- c(0.012663407, 0.029367501, 0.014456455, 0.012046011,
                  0.007477799, 0.189784408, 0.008738591, 0.015635544,
                  0.012787201, 0.013071539, 0.046268385, 0.006580823, 0.009102,
                  0.00760554, 0.047173998, 0.034356881, 0.137813902,
                  0.016876624, 0.065780114, 0.053775553, 0.228519883,
                  0.030117841)
#' @srrstats {G3.0, EA6.0, EA6.0e} Return values, single-valued objects.
  expect_equal(concstats_hhi(share_2018, normalized = TRUE), share_2018_hhi2,
               tolerance = .Machine$double.eps^0.25)
  expect_equal(concstats_hhi(x, normalized = TRUE),
               (sum(x ^ 2) - (1 / sum(x > 0))) / (1 - (1 / sum(x > 0))))
#' @srrstats {EA6.0, EA6.0a} Return values
  expect_true(is.numeric(share_2018), label = "numeric values returned")
})

## concstats_hhi_min

test_that("concstats_hhi_min function operates properly", {
#' @srrstats {G5.1} Data used to test, made generally available and run
#'  examples.
  x <- c(0.2, 0.3, 0.4, 0.1)
  x1 <- c(0.2, 0.3, 0.4, -0.1)
  x1b <- c()
  x2 <- c(0.2, 0.3, 0.4, 0.1, NA)
  x4 <- c(20, 30, 40, 10)
  x5 <- c(0.2, 0.3, 0.4, 0.10001)
  x6 <- c(0.2, 0.3, 0.4, 0.1, 0.0000000001)
  x8 <- c(-0.2, -0.3, -0.4, -0.100001)
  x9 <- c(NA, NA, NA, NA, NA, NA, NA, NA)
  xch <- c("a", "b", "c", "d", "e", "f", "g", "h")

  expect_true(any(is.na(x2)), all(!is.na(x2)))
  expect_vector(x, ptype = numeric(), size = 4)
  expect_equal(concstats_hhi_min(x2, na.rm = FALSE), NA_real_)
#' @srrstats {G5.2, G5.2a, G5.2b, G5.8, G5.8b} Edge test for data of
#'  unsupported types
  expect_error(concstats_hhi_min(xch, !is.numeric(xch)))
#' @srrstats {G5.0, G5.2, G5.2a, G5.2b, G5.8c} Edge test, Error on vector with
#'  all-`NA` fields
  expect_error(concstats_hhi_min(x9, na.rm = TRUE))
  expect_error(concstats_hhi_min(x8, na.rm = TRUE))
  expect_error(concstats_hhi_min(x1b, na.rm = TRUE))
  expect_error(concstats_hhi_min(x, na.rm = 0))
#' @srrstats {G3.0, EA6.0, EA6.0e} Return values, single-valued objects.
  act <- concstats_hhi_min(x)
  exp <- concstats_hhi_min(x4 / sum(x4))
  expect_equal(act, exp, tolerance = .Machine$double.eps^0.25)
  #' @srrstats {G3.0, G5.9, G5.9a} Adding trivial noise
  act <- concstats_hhi_min(x)
  exp <- concstats_hhi_min(x5)
  expect_equal(act, exp, tolerance = .Machine$double.eps^0.25)
#' @srrstats {G5.2, G5.2a, G5.2b, EA6.0, EA6.0e} Return values, single-valued
#'  objects
  expect_error(concstats_hhi_min(sum(x1), 1,
                                 tolerance = .Machine$double.eps^0.25))

})

test_that("concstats_hhi_min returns min of squared shares", {

  x <- c(0.2, 0.3, 0.4, 0.1)
  share_2018_hhi_min <- 0.04545455
  share_2018 <- c(0.012663407, 0.029367501, 0.014456455, 0.012046011,
                  0.007477799, 0.189784408, 0.008738591, 0.015635544,
                  0.012787201, 0.013071539, 0.046268385, 0.006580823, 0.009102,
                  0.00760554, 0.047173998, 0.034356881, 0.137813902,
                  0.016876624, 0.065780114, 0.053775553, 0.228519883,
                  0.030117841)
#' @srrstats {G3.0, EA6.0, EA6.0e} Return values, single-valued objects.
  expect_equal(concstats_hhi_min(share_2018), share_2018_hhi_min,
               tolerance = .Machine$double.eps^0.25)
  expect_equal(concstats_hhi_min(x), 1 / sum(x > 0))
#' @srrstats {EA6.0, EA6.0a} Return values
  expect_true(is.numeric(share_2018_hhi_min), label = "numeric values returned")
})

## concstats_hhi_d

test_that("concstats_hhi_d function operates properly", {
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

  expect_true(any(is.na(x2)), all(!is.na(x2)))
  expect_true(all(round(x) == 0), (abs(x) > 0 & abs(x) <= 1))
  expect_vector(x, ptype = numeric(), size = 4)
  expect_equal(concstats_hhi_d(x2, na.rm = FALSE), NA_real_)
#' @srrstats {G5.2, G5.2a, G5.2b, G5.8, G5.8b} Edge test for data of
#'  unsupported types
  expect_error(concstats_hhi_d(xch, !is.numeric(xch)))
#' @srrstats {G5.0, G5.2, G5.2a, G5.2b, G5.8c} Error and error messages on
#'  vector with all-`NA` fields
  expect_error(concstats_hhi_d(x9, na.rm = TRUE))
  expect_error(concstats_hhi_d(x8, na.rm = TRUE))
  expect_error(concstats_hhi_d(x1b, na.rm = TRUE))
  expect_error(concstats_hhi_d(x, na.rm = 0))
#' @srrstats {G3.0, EA6.0, EA6.0e} Return values single-valued objects.
  act <- concstats_hhi_d(x)
  exp <- concstats_hhi_d(x4 / sum(x4))
  expect_equal(act, exp, tolerance = .Machine$double.eps^0.25)
#' @srrstats {G3.0, G5.9, G5.9a} Adding trivial noise
  act <- concstats_hhi_d(x)
  exp <- concstats_hhi_d(x5)
  expect_equal(act, exp, tolerance = .Machine$double.eps^0.25)

#' @srrstats {G5.2, G5.2a, G5.2b, EA6.0, EA6.0e} Return values, single-valued
#'  objects
  expect_error(concstats_hhi_d(sum(x1), 1,
                               tolerance = .Machine$double.eps^0.25))

})

test_that("concstats_hhi_d returns dual of hhi", {

  x <- c(0.2, 0.3, 0.4, 0.1, 0)
  share_2018_hhi_d <- 0.6318321
  share_2018 <- c(0.012663407, 0.029367501, 0.014456455, 0.012046011,
                  0.007477799, 0.189784408, 0.008738591, 0.015635544,
                  0.012787201, 0.013071539, 0.046268385, 0.006580823, 0.009102,
                  0.00760554, 0.047173998, 0.034356881, 0.137813902,
                  0.016876624, 0.065780114, 0.053775553, 0.228519883,
                  0.030117841)

#' @srrstats {G3.0, EA6.0, EA6.0e} Return values, single-valued objects.
  expect_equal(concstats_hhi_d(share_2018), share_2018_hhi_d,
               tolerance = .Machine$double.eps^0.25)
  expect_equal(concstats_hhi_d(x), 1 - 1 / (sum(x > 0) * sum(x ^ 2)))
#' @srrstats {EA6.0, EA6.0a} Return values
  expect_true(is.numeric(share_2018_hhi_d), label = "numeric values returned")
})

## concstats_dom

test_that("concstats_dom function operates properly", {
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

  expect_true(any(is.na(x2)), all(!is.na(x2)))
  expect_true(all(round(x) == 0), (abs(x) > 0 & abs(x) <= 1))
  expect_vector(x, ptype = numeric(), size = 4)
  expect_equal(concstats_dom(x2, na.rm = FALSE), NA_real_)
#' @srrstats {G5.2, G5.2a, G5.2b, G5.8, G5.8b} Edge test for data of
#'  unsupported types
  expect_error(concstats_dom(xch, !is.numeric(xch)))
#' @srrstats {G5.0, G5.2, G5.2a, G5.2b, G5.8c} Error and error messages on
#'  vector with all-`NA` fields
  expect_error(concstats_dom(x9, na.rm = TRUE))
  expect_error(concstats_dom(x8, na.rm = TRUE))
  expect_error(concstats_dom(x1b, na.rm = TRUE))
  expect_error(concstats_dom(x, na.rm = 0))
#' @srrstats {G3.0, EA6.0, EA6.0e} Return values single-valued objects.
  act <- concstats_dom(x)
  exp <- concstats_dom(x4 / sum(x4))
  expect_equal(act, exp, tolerance = .Machine$double.eps^0.25)
#' @srrstats {G3.0, G5.9, G5.9a} Adding trivial noise
  act <- concstats_dom(x5)
  exp <- concstats_dom(x)
  expect_equal(act, exp, tolerance = .Machine$double.eps^0.25)
  act <- concstats_dom(x6)
  exp <- concstats_dom(x)
  expect_equal(act, exp, tolerance = .Machine$double.eps^0.25)
#' @srrstats {G5.2, G5.2a, G5.2b, EA6.0, EA6.0e} Return values, single-valued
#'  objects
  expect_error(concstats_dom(sum(x1), 1, tolerance = .Machine$double.eps^0.25))

})

test_that("concstats_dom returns dominance index", {

  x <- c(0.2, 0.3, 0.4, 0.1, 0)
  share_2018_dom <- 0.2903013
  share_2018 <- c(0.012663407, 0.029367501, 0.014456455, 0.012046011,
                  0.007477799, 0.189784408, 0.008738591, 0.015635544,
                  0.012787201, 0.013071539, 0.046268385, 0.006580823, 0.009102,
                  0.00760554, 0.047173998, 0.034356881, 0.137813902,
                  0.016876624, 0.065780114, 0.053775553, 0.228519883,
                  0.030117841)

#' @srrstats {G3.0, EA6.0, EA6.0e} Return values, single-valued objects.
  expect_equal(concstats_dom(share_2018), share_2018_dom,
               tolerance = .Machine$double.eps^0.25)
  expect_equal(concstats_dom(x), sum((x ^ 2 / (sum(x ^ 2))) ^ 2))
#' @srrstats {EA6.0, EA6.0a} Return values
  expect_true(is.numeric(share_2018_dom), label = "numeric values returned")
})

## concstats_sten

test_that("concstats_sten function operates properly", {
#' @srrstats {G5.1} Data used to test, made generally available and run
#'  examples.
  x <- c(0.2, 0.3, 0.4, 0.1)
  x1 <- c(0.2, 0.3, 0.5, -0.1)
  x1b <- c()
  x2 <- c(0.4, 0.3, 0.2, 0.1)
  x3 <- c(0.2, 0.3, 0.4, 0.1, NA)
  x4 <- c(20, 30, 40, 10)
  x5 <- c(0.2, 0.3, 0.4, 0.10001)
  x6 <- c(0.2, 0.3, 0.4, 0.1, 0.0000000001)
  x8 <- c(-0.2, -0.3, -0.4, -0.100001)
  x9 <- c(NA, NA, NA, NA, NA, NA, NA, NA)
  xch <- c("a", "b", "c", "d", "e", "f", "g", "h")

  expect_true(any(is.na(x3)), all(!is.na(x3)))
  expect_equal(sort(x, decreasing = TRUE), x2)
  expect_true(all(round(x) == 0), (abs(x) > 0 & abs(x) <= 1))
  expect_vector(x, ptype = numeric(), size = 4)
  expect_equal(concstats_sten(x3, na.rm = FALSE), NA_real_)
#' @srrstats {G5.2, G5.2a, G5.2b, G5.8, G5.8b} Edge test for data of
#'  unsupported types
  expect_error(concstats_sten(xch, !is.numeric(xch)))
#' @srrstats {G5.0, G5.2, G5.2a, G5.2b, G5.8c} Error and error messages on
#'  vector with all-`NA` fields
  expect_error(concstats_sten(x9, na.rm = TRUE))
  expect_error(concstats_sten(x8, na.rm = TRUE))
  expect_error(concstats_sten(x1b, na.rm = TRUE))
  #expect_error(concstats_sten(x, na.rm = 0))
#' @srrstats {G3.0, EA6.0, EA6.0e} Return values single-valued objects.
  act <- concstats_sten(x)
  exp <- concstats_sten(x2)
  expect_equal(act, exp, tolerance = .Machine$double.eps^0.25)
#' @srrstats {G3.0, G5.9, G5.9a} Adding trivial noise
  act <- concstats_sten(x)
  exp <- concstats_sten(x5)
  expect_equal(act, exp, tolerance = .Machine$double.eps^0.25)
  act <- concstats_sten(x)
  exp <- concstats_sten(x6)
  expect_equal(act, exp, tolerance = .Machine$double.eps^0.25)

#' @srrstats {G5.2, G5.2a, G5.2b, EA6.0, EA6.0e} Return values, single-valued
#'  objects
  expect_error(concstats_sten(sum(x1), 1, tolerance = .Machine$double.eps^0.25))

})

test_that("concstats_sten returns stenbacka index", {

  x <- c(0.4, 0.3, 0.2, 0.1)
  share_2018_sten <- 49.18984
  share_2018 <- c(0.012663407, 0.029367501, 0.014456455, 0.012046011,
                  0.007477799, 0.189784408, 0.008738591, 0.015635544,
                  0.012787201, 0.013071539, 0.046268385, 0.006580823, 0.009102,
                  0.00760554, 0.047173998, 0.034356881, 0.137813902,
                  0.016876624, 0.065780114, 0.053775553, 0.228519883,
                  0.030117841)

  #' @srrstats {G3.0, EA6.0, EA6.0e} Return values, single-valued objects.
  expect_equal(concstats_sten(share_2018), share_2018_sten,
               tolerance = .Machine$double.eps^0.25)
  expect_equal(concstats_sten(x), 0.5 * (1 - 1 * (x[1] ^ 2 - x[2] ^ 2)) * 100)
#' @srrstats {EA6.0, EA6.0a} Return values
  expect_true(is.numeric(share_2018_sten), label = "numeric values returned")
})

test_that("concstats_all_comp returns a data frame", {
#' @srrstats {G5.1} Data used to test, made generally available and run
#'  examples.
  x <- c(0.4, 0.2, 0.15, 0.1, 0.05, 0.07, 0.03)
  x1 <- c(0.2, 0.3, 0.5, -0.1)
  x1b <- c()
  x2 <- c(0.4, 0.15, 0.05, 0.1, 0.2, 0.03, 0.07)
  x3 <- c(0.2, 0.3, 0.4, 0.1, NA)
  x4 <- c(40, 20, 15, 10, 5, 7, 3)
  x5 <- c(0.4, 0.2, 0.15, 0.1, 0.05, 0.07, 0.029999999)
  x8 <- c(-0.2, -0.3, -0.4, -0.100001)
  x9 <- c(NA, NA, NA, NA, NA, NA, NA, NA)
  xch <- c("a", "b", "c", "d", "e", "f", "g", "h")
  dummy_df <- data.frame(Measure = rep(letters[1:5]), Value = c(1, 2, 3, 4, 5))

  expect_vector(x, ptype = numeric(), size = 7)
  expect_true(any(is.na(x3)), all(!is.na(x3)))
#' @srrstats {EA6.0, EA6.0a, EA6.0b, EA6.0c,  EA6.0d} Classes, dimensions, and
#'  types of objects
  expect_equal(ncol(dummy_df), 2)
  expect_equal(x, as.numeric(x4 / sum(x4)))
  expect_true(is.numeric(dummy_df$Value))
  expect_type(dummy_df$Value, "double")
  expect_type(dummy_df$Measure, "character")
  expect_identical(names(dummy_df), c("Measure", "Value"))
  expect_true(is.data.frame(concstats_all_comp(x)), "data.frame")
  expect_equal(concstats_all_comp(x3, na.rm = FALSE), NA_real_)
#' @srrstats {G5.2, G5.2a, G5.2b, G5.8, G5.8b} Edge test for data of
#'  unsupported types
  expect_error(concstats_all_comp(xch, !is.numeric(xch)))
#' @srrstats {G5.0, G5.2, G5.2a, G5.2b, G5.8c} Error and other messages/warnings
#' on vector with all-`NA` fields
  expect_error(concstats_all_comp(x9, na.rm = TRUE))
  expect_error(concstats_all_comp(x8, na.rm = TRUE))
  expect_error(concstats_all_comp(x1b, na.rm = TRUE))
  expect_error(concstats_all_comp(x, na.rm = 0))
#' @srrstats {G5.2, G5.2a, G5.2b, EA6.0, EA6.0e} Return values, single-valued
#'  objects
  expect_error(concstats_all_comp(sum(x1), 1,
                                  tolerance = .Machine$double.eps^0.25))
#' @srrstats {G3.0, EA6.0, EA6.0e} Return values single-valued objects.
  act <- concstats_all_comp(x)
  exp <- concstats_all_comp(x2)
  expect_equal(act, exp, tolerance = .Machine$double.eps^0.25)
  #' @srrstats {G3.0, G5.9, G5.9a} Adding trivial noise
  act <- concstats_all_comp(x)
  exp <- concstats_all_comp(x5)

})
