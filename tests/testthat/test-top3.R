## concstats_top3

test_that("concstats_top3 function operates properly", {
  #' @srrstats {G5.1} Data used to test, made generally available and run
  #'  examples.
  x <- c(0.2, 0.25, 0.4, 0.1, 0.05)
  x1 <- c(0.2, 0.3, 0.25, 0.05, -0.2)
  x1b <- c()
  x2 <- c(0.2, 0.3, 0.4, 0.1, NA)
  x3 <- c(0.4, 0.25, 0.2, 0.1, 0.05)
  x4 <- c(20, 25, 40, 10, 5)
  x5 <- c(0.2, 0.25, 0.4, 0.05, 0.10001)
  x6 <- c(0.2, 0.3, 0.4, 0.1, 0.0000000001)
  x7 <- c(0.2, 0.2499999999, 0.4, 0.1, 0.05)
  x8 <- c(-0.2, -0.3, -0.4, -0.100001)
  x9 <- c(NA, NA, NA, NA, NA, NA, NA, NA)

  expect_true(any(is.na(x2)), all(!is.na(x2)))
  expect_true(all(round(x) == 0), (abs(x) > 0 & abs(x) <= 1))
  expect_vector(x, ptype = numeric(), size = 5)
  expect_equal(concstats_top3(x2, na.rm = FALSE), NA_real_)
  expect_equal(sort(x, decreasing = TRUE), sort(x3, decreasing = TRUE))
  #' @srrstats {G5.2, G5.2a, G5.2b, G5.8, G5.8b} Edge test for data of
  #'  unsupported types
  expect_error(concstats_top3(xch, !is.numeric(xch)))
  #' @srrstats {G5.2, G5.2a, G5.2b, G5.8c} Error on vector with all-`NA` fields
  expect_error(concstats_top3(x9, na.rm = TRUE))
  expect_error(concstats_top3(x8, na.rm = TRUE))
  expect_error(concstats_top3(x1b, na.rm = TRUE))
  expect_error(concstats_top3(x, na.rm = 0))
  expect_error(concstats_top3(x1, as.logical(any(x < 0))))
  #' @srrstats {G3.0, EA6.0, EA6.0e} Testing values of single-valued objects.
  act <- concstats_top3(x)
  exp <- concstats_top3(x4 / sum(x4))
  expect_equal(act, exp, tolerance = .Machine$double.eps^0.25)
  #' @srrstats {G3.0, G5.9, G5.9a} Adding trivial noise
  act <- concstats_top3(x)
  exp <- concstats_top3(x3)
  expect_equal(act, exp, tolerance = .Machine$double.eps^0.25)
  act <- concstats_top3(x)
  exp <- concstats_top3(x7)
  expect_equal(act, exp, tolerance = .Machine$double.eps^0.25)
  #' @srrstats {G5.2, G5.2a, G5.2b, EA6.0, EA6.0e} Return values, single-valued
  #'  objects
  expect_error(concstats_top3(sum(x1), 1,
                              tolerance = .Machine$double.eps^0.25))
})

test_that("concstats_top3 returns sum of top 3 market shares", {

  x <- c(0.4, 0.3, 0.2, 0.1)
  x4 <- c(20, 25, 40, 10, 5)
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
  x4 <- sort(x4 / sum(x4, na.rm = TRUE), decreasing = TRUE)
  expect_equal(concstats_top3(x4), as.numeric(sum(x4[1:3], na.rm = TRUE) *100))
  #' @srrstats {EA6.0, EA6.0a} Return values
  expect_true(is.numeric(share_2018_top3), label = "numeric values returned")
})
