local_edition(3)

## concstats_entropy

test_that("concstats_entropy function operates properly", {
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
  expect_message(concstats_entropy(x2))

  expect_error(concstats_entropy(xch, !is.numeric(xch)))

  expect_error(concstats_entropy(x9, na.rm = TRUE))
  expect_error(concstats_entropy(x8, na.rm = TRUE))
  expect_error(concstats_entropy(x1b, na.rm = TRUE))
  expect_error(concstats_entropy(x2, na.rm = 0))
  expect_error(concstats_entropy(x2, normalized = 0))
  # digits argument
  expect_error(expect_int(x, digits = c(8, 0)))
  # convert to contiuous
  act <- concstats_entropy(x)
  exp <- concstats_entropy(x4 / sum(x4))
  expect_equal(act, exp, tolerance = .Machine$double.eps^0.25)
  # Adding trivial noise
  act <- concstats_entropy(x)
  exp <- concstats_entropy(x5)
  expect_equal(act, exp, tolerance = .Machine$double.eps^0.25)
  # test if sum x = 1
  expect_error(concstats_entropy(sum(x1), 1,
                                 tolerance = .Machine$double.eps^0.25))

})

test_that("concstats_entropy returns the unbiased entropy measure ", {

  x <- c(0.2, 0.25, 0.4, 0.1, 0.05)
  x3 <- c(0.4, 0.25, 0.2, 0.1, 0.05)
  x4 <- c(20, 25, 40, 10, 5)
  share_2018_ent <- 0.8024276
  share_2018 <- c(0.012663407, 0.029367501, 0.014456455, 0.012046011,
                  0.007477799, 0.189784408, 0.008738591, 0.015635544,
                  0.012787201, 0.013071539, 0.046268385, 0.006580823, 0.009102,
                  0.00760554, 0.047173998, 0.034356881, 0.137813902,
                  0.016876624, 0.065780114, 0.053775553, 0.228519883,
                  0.030117841)

  expect_equal(concstats_entropy(share_2018), share_2018_ent,
               tolerance = .Machine$double.eps^0.25)
  expect_equal(concstats_entropy(x),
               (sum(-x / sum(x) * log(x / sum(x), base = 2))
                / log(sum(x > 0), base = 2)))
  x4 <- x4 / sum(x4)
  expect_equal(concstats_entropy(x4),
               (sum(-x4 / sum(x4) * log(x4 / sum(x4), base = 2))
                / log(sum(x4 > 0), base = 2)))
  expect_true(is.numeric(share_2018_ent),
              label = "numeric values returned")
})

test_that("concstats_entropy returns the biased entropy measure ", {

  x <- c(0.2, 0.25, 0.4, 0.1, 0.05)
  x3 <- c(0.4, 0.25, 0.2, 0.1, 0.05)
  x4 <- c(20, 25, 40, 10, 5)
  share_2018_ent2 <- 3.578371
  share_2018 <- c(0.012663407, 0.029367501, 0.014456455, 0.012046011,
                  0.007477799, 0.189784408, 0.008738591, 0.015635544,
                  0.012787201, 0.013071539, 0.046268385, 0.006580823, 0.009102,
                  0.00760554, 0.047173998, 0.034356881, 0.137813902,
                  0.016876624, 0.065780114, 0.053775553, 0.228519883,
                  0.030117841)

  expect_equal(concstats_entropy(share_2018, normalized = FALSE),
               share_2018_ent2, tolerance = .Machine$double.eps^0.25)
  expect_equal(concstats_entropy(x, normalized = FALSE),
               sum(-x / sum(x) * log(x / sum(x), base = 2)))
  x4 <- x4 / sum(x4)
  expect_equal(concstats_entropy(x4, normalized = FALSE),
               sum(-x4 / sum(x4) * log(x4 / sum(x4), base = 2)))

  checkmate::qexpect(concstats_entropy(x),"N[0,)")

})
