local_edition(3)

## concstats_hhi_d

test_that("concstats_hhi_d function operates properly", {
  x <- c(0.2, 0.25, 0.4, 0.1, 0.05)
  x1 <- c(0.2, 0.3, 0.25, 0.05, -0.2)
  x1b <- c()
  x2 <- c(0.2, 0.3, 0.4, 0.1, NA)
  x4 <- c(20, 25, 40, 10, 5)
  x5 <- c(0.2, 0.25, 0.4, 0.05, 0.10001)
  x7 <- c(0.2, 0.2499999999, 0.4, 0.1, 0.05)
  x8 <- c(-0.2, -0.3, -0.4, -0.100001)
  x9 <- c(NA, NA, NA, NA, NA)
  xch <- c("a", "b", "c", "d", "e")

  expect_true(any(is.na(x2)), all(!is.na(x2)))
  expect_true(all(round(x) == 0), (abs(x) > 0 & abs(x) <= 1))
  expect_vector(x, ptype = numeric(), size = 5)
  expect_message(concstats_hhi_d(x2))

  expect_error(concstats_hhi_d(xch, !is.numeric(xch)))

  expect_error(concstats_hhi_d(x9, na.rm = TRUE))
  expect_error(concstats_hhi_d(x8, na.rm = TRUE))
  expect_error(concstats_hhi_d(x1b, na.rm = TRUE))
  expect_error(concstats_hhi_d(x, na.rm = 0))
  expect_error(concstats_hhi_d(x1, any(x1 < 0)))
  # digits argument
  expect_error(expect_int(x, digits = c(8, 0)))
  # convert to continuous
  act <- concstats_hhi_d(x)
  exp <- concstats_hhi_d(x4 / sum(x4))
  expect_equal(act, exp, tolerance = .Machine$double.eps^0.25)
  # adding trivial noise
  act <- concstats_hhi_d(x)
  exp <- concstats_hhi_d(x5)
  expect_equal(act, exp, tolerance = .Machine$double.eps^0.25)

  # test if sum x = 1
  expect_error(concstats_hhi_d(sum(x1), 1,
                               tolerance = .Machine$double.eps^0.25))

})

test_that("concstats_hhi_d returns dual of hhi", {

  x <- c(0.2, 0.25, 0.4, 0.1, 0.05)
  x4 <- c(20, 25, 40, 10, 5)
  share_2018_hhi_d <- 0.6318321
  share_2018 <- c(0.012663407, 0.029367501, 0.014456455, 0.012046011,
                  0.007477799, 0.189784408, 0.008738591, 0.015635544,
                  0.012787201, 0.013071539, 0.046268385, 0.006580823, 0.009102,
                  0.00760554, 0.047173998, 0.034356881, 0.137813902,
                  0.016876624, 0.065780114, 0.053775553, 0.228519883,
                  0.030117841)

  expect_equal(concstats_hhi_d(share_2018), share_2018_hhi_d,
               tolerance = .Machine$double.eps^0.25)
  expect_equal(concstats_hhi_d(x), 1 - 1 / (sum(x > 0) * sum(x ^ 2)))

  expect_equal(concstats_hhi_d(x4),
               1 - 1 /sum(((x4/sum(x4,na.rm = TRUE)) > 0) *
                            sum((x4 / sum(x4, na.rm = TRUE))^ 2)))

  checkmate::qexpect(concstats_hhi_d(x),"N[0,)")
})
