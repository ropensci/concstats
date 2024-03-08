local_edition(3)

## concstats_shares

test_that("concstats_shares function operates properly", {
  #' @srrstats {G5.1} Data used to test, made generally available and run
  #'  examples.
  x <- c(538572286.08, 481096.77, 161914143.03, 128796268.59, 69055940.72)
  x1 <- c(538572286.08, 481096.77, 161914143.03, 128796268.59, -69055940.72)
  x2 <- c(538572286.08, 481096.77, 161914143.03, 128796268.59, 69055940.72, NA)
  x3 <- c(0.2, 0.25, 0.4, 0.1, 0.05)
  x4 <- c(20, 25, 40, 10, 5)
  x5 <- c(0.2, 0.25, 0.4, 0.05, 0.10001)
  x6 <- c(0.2, 0.2499999999, 0.4, 0.1, 0.05)
  x9 <- c(NA, NA, NA, NA, NA)
  xch <- c("a", "b", "c", "d", "e")
  x7 <- c(0.5991994446, 0.0005352539, 0.1801408410, 0.1432948828, 0.0768295777)

  expect_true(any(is.na(x2)), all(!is.na(x2)))
  expect_vector(x, ptype = numeric(), size = 5)
  expect_equal(concstats_shares(x2, na.rm = FALSE), NA_real_)
  #' @srrstats {G5.2, G5.2a, G5.2b, G5.8, G5.8b} Edge test for data of
  #'  unsupported types
  expect_error(concstats_shares(xch, !is.numeric(xch)))
  #' @srrstats {G5.0, G5.2, G5.2a, G5.2b, G5.8c} Error and error messages on
  #'  vector with all-`NA` fields
  expect_error(concstats_shares(x9, na.rm = TRUE))
  expect_error(concstats_shares(x, na.rm = 0))
  expect_error(concstats_dom(x1, any(x1 < 0)))
  #' @srrstats {G3.0, EA6.0, EA6.0e} Return values single-valued objects.
  act <- concstats_shares(x, digits = NULL)
  exp <- concstats_shares(x7)
  expect_equal(act, exp, tolerance = .Machine$double.eps^0.25)
  act <- concstats_shares(x, digits = NULL)
  exp <- concstats_shares(as.numeric(x / sum(x, na.rm = TRUE)))
  expect_equal(act, exp, tolerance = .Machine$double.eps^0.25)
  #' @srrstats {G3.0, G5.9, G5.9a} Adding trivial noise
  act <- concstats_dom(x3)
  exp <- concstats_dom(x6)
  expect_equal(act, exp, tolerance = .Machine$double.eps^0.25)

})

test_that("concstats_shares returns a vector of individual shares", {

  x3 <- c(0.2, 0.25, 0.4, 0.1, 0.05)
  x4 <- c(20, 25, 40, 10, 5)
  x <- c(538572286.08, 481096.77, 161914143.03, 128796268.59, 69055940.72)
  out <- c(0.59920, 0.00054, 0.18014, 0.14329, 0.07683)

  #' @srrstats {G3.0, EA6.0, EA6.0e} Return values, single-valued objects.
  expect_equal(concstats_shares(x, digits = 5), out,
               tolerance = .Machine$double.eps^0.25)
  expect_equal(concstats_shares(x), x / sum(x, na.rm = TRUE))
  #' @srrstats {EA6.0, EA6.0a} Return values
  expect_true(is.numeric(out), label = "numeric values returned")
})
