local_edition(3)


## concstats_top3_df

test_that("concstats_top3_df function operates properly", {
  #' @srrstats {G5.1} Data used to test, made generally available and run
  #'  examples.
  id <- c(1, 2, 3, 4, 5)
  x <- c(0.2, 0.25, 0.4, 0.1, 0.05)
  x1 <- c(0.2, 0.3, 0.25, 0.05, -0.2)
  x2 <- c(0.2, 0.3, 0.4, 0.1, NA)
  x3 <- c(0.4, 0.25, 0.2, 0.1, 0.5)
  x4 <- c(20, 25, 40, 10, 5)
  x5 <- c(0.2, 0.25, 0.4, 0.05, 0.10001)
  x6 <- c(0.2, 0.3, 0.4, 0.1, 0.0000000001)
  x7 <- c(0.35, 0.299999999, 0.2, 0.1, 0.05)
  x8 <- c(-0.2, -0.3, -0.4, -0.100001)
  x9 <- c(NA, NA, NA, NA, NA)
  xch <- c("a", "b", "c", "d", "e")

  test_df <- data.frame(id, x)
  test_df1 <- data.frame(id, x1)
  test_df2 <- data.frame(id, x2)
  test_df3 <- data.frame(id, x3)
  test_df4 <- data.frame(id, x4)
  test_df5 <- data.frame(id, x5)
  test_df7 <- data.frame(id, x7)
  test_df9 <- data.frame(id, x9)
  test_dfch <- data.frame(id, xch)
  tolerance <- 0.01

  expect_s3_class(test_df, "data.frame")
  expect_true(any(is.na(test_df2)), all(!is.na(test_df2)))
  expect_true(all(round(test_df5$x5) == 0), (abs(test_df5$x5) > 0 &
                                              abs(test_df5$x5) <= 1))

  expect_equal(concstats_top3_df(test_df2, na.rm = FALSE), NA_real_)
  expect_equal(ncol(test_df), 2)

  #' @srrstats {G5.2, G5.2a, G5.2b, G5.8, G5.8b} Edge test for data of
  #'  unsupported types
  expect_error(concstats_top3_df(test_dfch, !is.numeric(test_dfch$y)))
  #' @srrstats {G5.2, G5.2a, G5.2b, G5.8c} Error on vector with all-`NA` fields
  expect_error(concstats_top3_df(test_df9, na.rm = TRUE, class = data.frame()))
  expect_error(concstats_top3_df(test_df2, na.rm = FALSE|TRUE))
  expect_error(concstats_top3_df(test_df, na.rm = 0))
  expect_error(concstats_top3_df(test_df1, as.logical(any(x < 0))))


  #' @srrstats {G3.0, G5.9, G5.9a} Adding trivial noise
  expect_equal(sum(test_df5$x5), sum(test_df$x),
               tolerance = .Machine$double.eps^0.25 )

  #' @srrstats {G5.2, G5.2a, G5.2b, EA6.0, EA6.0e} Return values, single-valued
  #'  objects
  expect_error(concstats_top3_df(sum(test_df1[ ,y]), 1,
                             tolerance = .Machine$double.eps^0.25))

})

test_that("concstats_top3_df returns top market share", {

  x4 <- c(20, 25, 40, 10, 5)
  id <- c(1, 2, 3, 4, 5)
  test_df4 <- data.frame(id, x4)
  top3_df <- concstats_top3_df(test_df4, "x4")
  share_2018_top <- 22.85199
  share_2018 <- c(0.012663407, 0.029367501, 0.014456455, 0.012046011,
                  0.007477799, 0.189784408, 0.008738591, 0.015635544,
                  0.012787201, 0.013071539, 0.046268385, 0.006580823, 0.009102,
                  0.00760554, 0.047173998, 0.034356881, 0.137813902,
                  0.016876624, 0.065780114, 0.053775553, 0.228519883,
                  0.030117841)

  #' @srrstats {G3.0, EA6.0, EA6.0e} Return values, single-valued objects.
  expect_equal(dim(top3_df), c(3,2))
  #expect_equal(concstats_top(x), x[1] * 100)
  #' @srrstats {EA6.0, EA6.0a} Return values
  expect_true(is.data.frame(test_df4), label = "dataframe returned")
})
