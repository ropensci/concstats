## concstats_firm

test_that("concstats_firm function operates properly", {
  x <- c(0.2, 0.25, 0.4, 0.1, 0.05)
  x1 <- c(0.2, 0.3, 0.25, 0.05, -0.2)
  x1b <- c()
  x2 <- c(0.2, 0.3, 0.4, 0.1, NA)
  x4 <- c(20, 25, 40, 10, 5)
  x5 <- c(0.2, 0.25, 0.4, 0.05, 0.10001)
  x6 <- c(0.2, 0.3, 0.4, 0.1, 0.0000000001)
  x7 <- c(0.2, 0.2499999999, 0.4, 0.1, 0.05)
  x8 <- c(-0.2, -0.3, -0.4, -0.100001)
  x9 <- c(NA, NA, NA, NA, NA, NA, NA, NA)

  expect_true(any(is.na(x2)), all(!is.na(x2)))
  expect_true(all(round(x) == 0), (abs(x) > 0 & abs(x) <= 1))
  expect_vector(x, ptype = numeric(), size = 5)
  expect_message(concstats_firm(x2))

  expect_error(concstats_firm(xch, !is.numeric(xch)))

  expect_error(concstats_firm(x9, na.rm = TRUE))
  expect_error(concstats_firm(x8, na.rm = TRUE))
  expect_error(concstats_firm(x1b, na.rm = TRUE))
  expect_error(concstats_firm(x, na.rm = 0))
  expect_error(concstats_firm(x1, any(x1 < 0)))
  # convert to continuous
  act <- concstats_firm(x)
  exp <- concstats_firm(x4 / sum(x4))
  expect_equal(act, exp, tolerance = .Machine$double.eps^0.25)
  # Adding trivial noise
  act <- concstats_firm(x5)
  exp <- concstats_firm(x)
  expect_equal(act, exp, tolerance = .Machine$double.eps^0.25)
  act <- concstats_firm(x7)
  exp <- concstats_firm(x)
  expect_equal(act, exp, tolerance = .Machine$double.eps^0.25)

  # test if sum x = 1
  expect_error(concstats_firm(sum(x1), 1,
                                tolerance = .Machine$double.eps^0.25))

})

test_that("concstats_firm returns numbers equivalent", {

  x <- c(0.2, 0.25, 0.4, 0.1, 0.05)
  x4 <- c(20, 25, 40, 10, 5)
  share_2018_firm <- 22
  share_2018 <- c(0.012663407, 0.029367501, 0.014456455, 0.012046011,
                  0.007477799, 0.189784408, 0.008738591, 0.015635544,
                  0.012787201, 0.013071539, 0.046268385, 0.006580823, 0.009102,
                  0.00760554, 0.047173998, 0.034356881, 0.137813902,
                  0.016876624, 0.065780114, 0.053775553, 0.228519883,
                  0.030117841)

  expect_equal(concstats_firm(share_2018), share_2018_firm,
               tolerance = .Machine$double.eps^0.25)
  expect_equal(concstats_firm(x), sum(x > 0, na.rm = TRUE))
  expect_equal(concstats_firm(x4), sum(x4 > 0, na.rm = TRUE))

  checkmate::qexpect(concstats_firm(x),"i1")

})
