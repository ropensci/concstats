## concstats_gini

test_that("concstats_gini function operates properly", {
  #' @srrstats {G5.1} Data used to test, made generally available and run
  #' examples.
  x <- c(0.2, 0.25, 0.4, 0.1, 0.05)
  x1 <- c(0.2, 0.3, 0.25, 0.05, -0.2)
  x1b <- c()
  x2 <- c(0.2, 0.3, 0.4, 0.1, NA)
  x3 <- c(0.05, 0.1, 0.2, 0.25, 0.4)
  x4 <- c(20, 25, 40, 10, 5)
  x5 <- c(0.2, 0.25, 0.4, 0.05, 0.10001)
  x7 <- c(0.2, 0.2499999999, 0.4, 0.1, 0.05)
  x8 <- c(-0.2, -0.3, -0.4, -0.100001)
  x9 <- c(NA, NA, NA, NA, NA)
  xch <- c("a", "b", "c", "d", "e")

  expect_true(any(is.na(x2)), all(!is.na(x2)))
  expect_true(all(round(x) == 0), (abs(x) > 0 & abs(x) <= 1))
  expect_vector(x, ptype = numeric(), size = 5)
  expect_equal(concstats_gini(x2, na.rm = FALSE), NA_real_)
  expect_equal(sort(x), x3)
  #' @srrstats {G5.2, G5.2a, G5.2b, G5.8, G5.8b} Edge test for data of
  #'  unsupported types
  expect_error(concstats_gini(xch, !is.numeric(xch)))
  #' @srrstats {G5.2, G5.2a, G5.2b, G5.8c} Error on vector with all-`NA` fields
  expect_error(concstats_gini(x9, na.rm = TRUE))
  expect_error(concstats_gini(x8, na.rm = TRUE))
  expect_error(concstats_gini(x1b, na.rm = TRUE))
  expect_error(concstats_gini(x, na.rm = 0))
  expect_error(concstats_gini(x, normalized = 0))
  #' @srrstats {G3.0, EA6.0, EA6.0e} Return values, single-valued objects.
  act <- concstats_gini(x)
  exp <- concstats_gini(x4 / sum(x4))
  expect_equal(act, exp, tolerance = .Machine$double.eps^0.25)
  #' @srrstats {G3.0, G5.9, G5.9a} Adding trivial noise
  act <- concstats_gini(x)
  exp <- concstats_gini(x5)
  expect_equal(act, exp, tolerance = .Machine$double.eps^0.25)
  expect_error(concstats_gini(sum(x1), 1, tolerance = .Machine$double.eps^0.25))

})

test_that("concstats_gini returns the gini measure", {

  x <- c(0.2, 0.25, 0.4, 0.1, 0.05)
  x3 <- c(0.4, 0.25, 0.2, 0.1, 0.05)
  x4 <- c(40, 25, 20, 10, 5)
  share_2018_gini <- 0.5793415
  share_2018 <- c(0.012663407, 0.029367501, 0.014456455, 0.012046011,
                  0.007477799, 0.189784408, 0.008738591, 0.015635544,
                  0.012787201, 0.013071539, 0.046268385, 0.006580823, 0.009102,
                  0.00760554, 0.047173998, 0.034356881, 0.137813902,
                  0.016876624, 0.065780114, 0.053775553, 0.228519883,
                  0.030117841)

  #' @srrstats {G3.0, EA6.0, EA6.0e} Return values, single-valued objects.
  expect_equal(concstats_gini(share_2018, normalized = FALSE), share_2018_gini,
               tolerance = .Machine$double.eps^0.25)
  x <- sort(x)
  expect_equal(concstats_gini(x, normalized = FALSE),
               2 * sum(x * seq_len(length(x))
                       / length(x) * sum(x)) - 1 - (1 / length(x)))
  x4 <- sort(x4 / sum(x4))
  expect_equal(concstats_gini(x4, normalized = FALSE),
               2 * sum(x4 * seq_len(length(x4))
                       / length(x4) * sum(x4)) - 1 - (1 / length(x4)))
  #' @srrstats {EA6.0, EA6.0a} Return values
  expect_true(is.numeric(share_2018_gini), label = "numeric values returned")
})

test_that("concstats_gini returns the normalized gini measure", {

  x <- c(0.2, 0.25, 0.4, 0.1, 0.05)
  x3 <- c(0.4, 0.25, 0.2, 0.1, 0.05)
  x4 <- c(20, 25, 40, 10, 5)
  share_2018_gini2 <- 0.6069292
  share_2018 <- c(0.012663407, 0.029367501, 0.014456455, 0.012046011,
                  0.007477799, 0.189784408, 0.008738591, 0.015635544,
                  0.012787201, 0.013071539, 0.046268385, 0.006580823, 0.009102,
                  0.00760554, 0.047173998, 0.034356881, 0.137813902,
                  0.016876624, 0.065780114, 0.053775553, 0.228519883,
                  0.030117841)

  #' @srrstats {G3.0, EA6.0, EA6.0e} Return values, single-valued objects.
  expect_equal(concstats_gini(share_2018, normalized = TRUE), share_2018_gini2,
               tolerance = .Machine$double.eps^0.25)
  x <- sort(x)
  expect_equal(concstats_gini(x, normalized = TRUE),
               length(x) / (length(x) - 1) *
                 (2 * sum(x * seq_len(length(x)) / length(x) *
                            sum(x)) - 1 - (1 / length(x))))
  x4 <- sort(x4 / sum(x4))
  expect_equal(concstats_gini(x4, normalized = TRUE),
               length(x4) / (length(x4) - 1) *
                 (2 * sum(x4 * seq_len(length(x4)) / length(x4) *
                            sum(x4)) - 1 - (1 / length(x4))))
  #' @srrstats {EA6.0, EA6.0a} Return values
  expect_true(is.numeric(share_2018_gini2), label = "numeric values returned")
})
