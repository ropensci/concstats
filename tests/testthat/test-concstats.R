local_edition(3)

# concstats_concstats

test_that("concstats_concstats function operates properly", {
  x <- c(0.2, 0.3, 0.4, 0.1)
  x1 <- c(0.2, 0.3, 0.4, -0.1)
  x1b <- c()
  x2 <- c(0.4, 0.3, 0.2, 0.1, NA)
  x3 <- c(0.4, 0.3, 0.2, 0.1)
  x4 <- c(20, 30, 40, 10)
  x5 <- c(0.2, 0.3, 0.4, 0.10001)
  x6 <- c(0.2, 0.3, 0.4, 0.1, 0.0000000001)
  x8 <- c(-0.2, -0.3, -0.4, -0.100001)
  x9 <- c(NA, NA, NA, NA, NA, NA, NA, NA)
  xch <- c("a", "b", "c", "d", "e", "f", "g", "h")


  expect_true(any(is.na(x2)), all(!is.na(x2)))

  expect_true(all(round(x3) == 0), (abs(x3) > 0 & abs(x3) <= 1))
  expect_vector(x3, ptype = numeric(), size = 4)
  expect_true(is.numeric(x3), label = "numeric values returned")
  expect_message(concstats_concstats(x2))
  expect_equal(sort(x, decreasing = TRUE), x3)

  expect_error(concstats_concstats(xch, !is.numeric(xch)))

  expect_error(concstats_concstats(x9, na.rm = TRUE))
  #expect_error(concstats_concstats(x8, na.rm = TRUE))
  expect_error(concstats_concstats(x1b, na.rm = TRUE))
  expect_error(concstats_concstats(x, na.rm = 0))
  # digits argument
  expect_error(expect_int(x, digits = c(8, -4)))
  # converting integer to decimal
  act <- concstats_concstats(x)
  exp <- concstats_concstats(x4 / sum(x4))
  expect_equal(act, exp, tolerance = .Machine$double.eps^0.25)

  # Adding trivial noise
  act <- concstats_concstats(x)
  exp <- concstats_concstats(x5)
  expect_equal(act, exp, tolerance = .Machine$double.eps^0.25)

  expect_error(concstats_concstats(sum(x1), 1,
                                       tolerance = .Machine$double.eps^0.2))

})

test_that("concstats_firm returns number of firms", {
  x <- c(0.4, 0.3, 0.2, 0.1)
  share_2018_firm <- 22
  share_2018 <- c(0.012663407, 0.029367501, 0.014456455, 0.012046011,
                  0.007477799, 0.189784408, 0.008738591, 0.015635544,
                  0.012787201, 0.013071539, 0.046268385, 0.006580823, 0.009102,
                  0.00760554, 0.047173998, 0.034356881, 0.137813902,
                  0.016876624, 0.065780114, 0.053775553, 0.228519883,
                  0.030117841)

  expect_equal(concstats_firm(share_2018), share_2018_firm,
               tolerance = .Machine$double.eps^0.25)
  expect_equal(concstats_firm(x), sum(x > 0))

  expect_true(is.numeric(share_2018_firm), label = "numeric values returned")
})

test_that("concstats_nrs_equ returns numbers equivalent", {

  x <- c(0.4, 0.3, 0.2, 0.1)
  share_2018_nrs <- 8.099694
  share_2018 <- c(0.012663407, 0.029367501, 0.014456455, 0.012046011,
                  0.007477799, 0.189784408, 0.008738591, 0.015635544,
                  0.012787201, 0.013071539, 0.046268385, 0.006580823, 0.009102,
                  0.00760554, 0.047173998, 0.034356881, 0.137813902,
                  0.016876624, 0.065780114, 0.053775553, 0.228519883,
                  0.030117841)

  expect_equal(concstats_nrs_eq(share_2018), share_2018_nrs,
               tolerance = .Machine$double.eps^0.25)
  expect_equal(concstats_nrs_eq(x), 1 / sum(x ^ 2))
  expect_true(is.numeric(share_2018_nrs), label = "numeric values returned")
})

test_that("concstats_top returns top market share", {

  x <- c(0.4, 0.3, 0.2, 0.1)
  share_2018_top <- 22.85199
  share_2018 <- c(0.012663407, 0.029367501, 0.014456455, 0.012046011,
                  0.007477799, 0.189784408, 0.008738591, 0.015635544,
                  0.012787201, 0.013071539, 0.046268385, 0.006580823, 0.009102,
                  0.00760554, 0.047173998, 0.034356881, 0.137813902,
                  0.016876624, 0.065780114, 0.053775553, 0.228519883,
                  0.030117841)

  expect_equal(concstats_top(share_2018), share_2018_top,
               tolerance = .Machine$double.eps^0.25)
  expect_equal(concstats_top(x), x[1] * 100)
  expect_true(is.numeric(share_2018_top), label = "numeric values returned")
})

test_that("concstats_top3 returns sum of top 3 market shares", {

  x <- c(0.4, 0.3, 0.2, 0.1)
  share_2018_top3 <- 55.61182
  share_2018 <- c(0.012663407, 0.029367501, 0.014456455, 0.012046011,
                  0.007477799, 0.189784408, 0.008738591, 0.015635544,
                  0.012787201, 0.013071539, 0.046268385, 0.006580823, 0.009102,
                  0.00760554, 0.047173998, 0.034356881, 0.137813902,
                  0.016876624, 0.065780114, 0.053775553, 0.228519883,
                  0.030117841)

  expect_equal(concstats_top3(share_2018), share_2018_top3,
               tolerance = .Machine$double.eps^0.25)
  expect_equal(concstats_top3(x), sum(x[1:3], na.rm = TRUE) * 100)
  expect_true(is.numeric(share_2018_top3), label = "numeric values returned")
})

test_that("concstats_top5 returns sum of top 5 market shares", {

  x <- c(0.4, 0.2, 0.15, 0.15, 0.1)
  share_2018_top5 <- 67.56739
  share_2018 <- c(0.012663407, 0.029367501, 0.014456455, 0.012046011,
                  0.007477799, 0.189784408, 0.008738591, 0.015635544,
                  0.012787201, 0.013071539, 0.046268385, 0.006580823, 0.009102,
                  0.00760554, 0.047173998, 0.034356881, 0.137813902,
                  0.016876624, 0.065780114, 0.053775553, 0.228519883,
                  0.030117841)

  expect_equal(concstats_top5(share_2018), share_2018_top5,
               tolerance = .Machine$double.eps^0.25)
  expect_equal(concstats_top5(x), sum(x[1:5], na.rm = TRUE) * 100)

  expect_true(is.numeric(share_2018_top5), label = "numeric values returned")
})

test_that("concstats_hhi returns sum of squared shares as decimal", {

  x <- c(0.4, 0.2, 0.15, 0.15, 0.1)
  share_2018_hhi <- 0.1234614
  share_2018 <- c(0.012663407, 0.029367501, 0.014456455, 0.012046011,
                  0.007477799, 0.189784408, 0.008738591, 0.015635544,
                  0.012787201, 0.013071539, 0.046268385, 0.006580823, 0.009102,
                  0.00760554, 0.047173998, 0.034356881, 0.137813902,
                  0.016876624, 0.065780114, 0.053775553, 0.228519883,
                  0.030117841)

  expect_equal(concstats_hhi(share_2018), share_2018_hhi,
               tolerance = .Machine$double.eps^0.25)
  expect_equal(concstats_hhi(x), sum(x ^ 2))

  expect_true(is.numeric(share_2018_hhi), label = "numeric values returned")
})

test_that("concstats_entropy returns the entropy measure", {

  x <- c(0.4, 0.2, 0.15, 0.1, 0.05, 0.07, 0.03)
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

  expect_true(is.numeric(share_2018_ent), label = "numeric values returned")
})

  test_that("concstats_palma returns the alternative palma measure", {

  x <- c(0.2, 0.3, 0.5)
  share_2018_palma <- 6.174089
  share_2018 <- c(0.012663407, 0.029367501, 0.014456455, 0.012046011,
                  0.007477799, 0.189784408, 0.008738591, 0.015635544,
                  0.012787201, 0.013071539, 0.046268385, 0.006580823, 0.009102,
                  0.00760554, 0.047173998, 0.034356881, 0.137813902,
                  0.016876624, 0.065780114, 0.053775553, 0.228519883,
                  0.030117841)

  expect_equal(concstats_palma(share_2018), share_2018_palma,
               tolerance = .Machine$double.eps^0.25)
  expect_equal(concstats_palma(x),
               sum(x
                   [cut(x, stats::quantile(x, probs = seq(0, 1, 0.1)),
                        include.lowest = TRUE, labels = FALSE) > 9]) /
                 sum(x
                     [cut(x, stats::quantile(x, probs = seq(0, 1, 0.1)),
                          include.lowest = TRUE, labels = FALSE) <= 4]))

  expect_true(is.numeric(share_2018_palma), label = "numeric values returned")
})

test_that("concstats_concstats returns a data frame", {

  x <- c(0.4, 0.2, 0.15, 0.1, 0.05, 0.07, 0.03)
  x3 <- c(0.2, 0.3, 0.4, 0.1, NA)
  x4 <- c(10, 30, 40, 20)
  dummy_df <- data.frame(Measure = rep(letters[1:5]), Value = c(1, 2, 3, 4, 5))

  expect_vector(x, ptype = numeric(), size = 7)
  expect_true(any(is.na(x3)), all(!is.na(x3)))

  expect_equal(ncol(dummy_df), 2)
  expect_true(is.numeric(dummy_df$Value))
  expect_type(dummy_df$Value, "double")
  expect_type(dummy_df$Measure, "character")
  expect_identical(names(dummy_df), c("Measure", "Value"))
  expect_true(is.data.frame(concstats_concstats(x)), "data.frame")

})
