local_edition(3)

## concstats_hhi

test_that("concstats_hhi function operates properly", {
  #' @srrstats {G5.1} Data used to test, made generally available and run
  #'  examples.
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

  #' @srrstats {G5.3} Expected to return objects containing no missing (`NA`)
  expect_true(any(is.na(x2)), all(!is.na(x2)))

  expect_true(all(round(x) == 0), (abs(x) > 0 & abs(x) <= 1))
  expect_vector(x, ptype = double(), size = 5)
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
  expect_error(concstats_hhi(x1, as.logical(any(x < 0))))
  #' @srrstats {G3.0, EA6.0, EA6.0e} Return values single-valued objects.
  act <- concstats_hhi(x)
  exp <- concstats_hhi(x4 / sum(x4))
  expect_equal(act, exp, tolerance = .Machine$double.eps^0.25)
  #' @srrstats {G3.0, G5.9, G5.9a} Adding trivial noise
  act <- concstats_hhi(x)
  exp <- concstats_hhi(x5)
  expect_equal(act, exp, tolerance = .Machine$double.eps^0.25)
  act <- concstats_hhi(x)
  exp <- concstats_hhi(x7)
  expect_equal(act, exp, tolerance = .Machine$double.eps^0.25)

  #' @srrstats {G5.2, G5.2a, G5.2b, EA6.0, EA6.0e} Return values, single-valued
  #'  objects
  expect_error(concstats_hhi(sum(x1), 1, tolerance = .Machine$double.eps^0.25))

})

test_that("concstats_hhi returns sum of squared shares as decimal", {

  #' @srrstats {G5.5, G5.4, G5.4a, G5.4c} Stored values (share_2018_) and vector
  #'  are drawn from [this paper](https://doi.org/10.1515/zfgg-2022-0002),
  #'   page 26/27
  x3 <- c(0.4, 0.25, 0.2, 0.1, 0.05)
  x4 <- c(20, 25, 40, 10, 5)
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
  expect_equal(concstats_hhi(x3), sum(x3 ^ 2, na.rm = TRUE))
  expect_equal(concstats_hhi(x4), sum((x4 / sum(x4, na.rm = TRUE))^2))
  #' @srrstats {EA6.0, EA6.0a} Return values
  expect_true(is.numeric(share_2018_hhi), label = "numeric values returned")

  #' @srrstats {G5.4, G5.4b} Correctness test from hhi package can be checked
  #'  just visually in the plot
  #'   (https://joss.theoj.org/papers/10.21105/joss.00828)

  us_2012_hhi <- 0.136192
  us_2012 <- c(1.3, 12.3, 11.5, 9.7, 4.5, 4.2, 0.6, 22.4, 17.9, 7.3, 8.3)
  expect_equal(concstats_hhi(us_2012), us_2012_hhi,
               tolerance = .Machine$double.eps^0.25)
})

test_that("concstats_hhi returns normalized hhi", {

  x <- c(0.2, 0.3, 0.4, 0.1)
  x4 <- c(20, 25, 40, 10, 5)
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
  expect_equal(concstats_hhi(x4, normalized = TRUE),
               as.numeric(sum((x4/sum(x4,na.rm = TRUE))^2) - (1 / sum(x4 > 0))) /
                            +                (1 - (1 / sum(x4 > 0))))
  #' @srrstats {EA6.0, EA6.0a} Return values
  expect_true(is.numeric(share_2018), label = "numeric values returned")
})

# loans_2018 <- c(3931577688408.00, 3512006440.00, 1181973244136.00, 940212760717.00,
#                 504108367226.00, 553126599129.00, 10349042811855.00, 85453595503.00,
#                 112921714388.00, 1080798333076.00, 994455032421.00, 1747108722293.00,
#                 868778394702.00, 430724585523.00, 636935285927.00, 776207212859.00,
#                 99060073546.00, 110683978608.00, 267147638455.00, 4250655893824.00,
#                 920406175495.00, 409127381045.00, 2096325112491.00, 760860195590.00,
#                 363222923884.00, 3511443628715.00, 3023084817958.00, 601839961663.00,
#                 12092000652237.00, 2036893637665.82)

