local_edition(3)

## concstats_top

test_that("concstats_top function operates properly", {
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
  expect_message(concstats_top3(x2))
  expect_equal(sort(x, decreasing = TRUE), sort(x3, decreasing = TRUE))

  expect_error(concstats_top(xch, !is.numeric(xch)))

  expect_error(concstats_top(x9, na.rm = TRUE))
  expect_error(concstats_top(x8, na.rm = TRUE))
  expect_error(concstats_top(x1b, na.rm = TRUE))
  expect_error(concstats_top(x, na.rm = 0))
  expect_error(concstats_top(x1, as.logical(any(x < 0))))
  # digits argument
  expect_error(expect_int(x, digits = c(8, 0)))
  # convert to continuous
  act <- concstats_top(x)
  exp <- concstats_top(x4 / sum(x4))
  expect_equal(act, exp, tolerance = .Machine$double.eps^0.25)
  # Adding trivial noise
  act <- concstats_top(x5)
  exp <- concstats_top(x)
  expect_equal(act, exp, tolerance = .Machine$double.eps^0.25)
  act <- concstats_top(x7)
  exp <- concstats_top(x)
  expect_equal(act, exp, tolerance = .Machine$double.eps^0.25)
  # test if sum x = 1
  expect_error(concstats_top(sum(x1), 1,
                             tolerance = .Machine$double.eps^0.25))

})

test_that("concstats_top returns top market share", {

  x3 <- c(0.4, 0.25, 0.2, 0.1, 0.05)
  x4 <- c(20, 25, 40, 10, 5)
  share_2018_top <- 22.85199
  share_2018 <- c(0.012663407, 0.029367501, 0.014456455, 0.012046011,
                  0.007477799, 0.189784408, 0.008738591, 0.015635544,
                  0.012787201, 0.013071539, 0.046268385, 0.006580823, 0.009102,
                  0.00760554, 0.047173998, 0.034356881, 0.137813902,
                  0.016876624, 0.065780114, 0.053775553, 0.228519883,
                  0.030117841)

  expect_equal(concstats_top(share_2018), share_2018_top,
               tolerance = .Machine$double.eps^0.25)
  expect_equal(concstats_top(x3), x3[1] * 100)
  x4 <- sort(x4 / sum(x4, na.rm = TRUE), decreasing = TRUE)
  expect_equal(concstats_top(x4), as.numeric(x4[1] *100))

  checkmate::qexpect(concstats_top(x4),"N[0,)")
})
