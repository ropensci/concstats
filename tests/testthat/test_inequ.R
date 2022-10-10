local_edition(3)

## concstats_inequ

test_that("concstats_inequ function operates / switches properly", {
#' @srrstats {G5.1} Data used to test, made generally available and run examples.
  x <- c(0.2, 0.3, 0.4, 0.1)
  x1 <- c(0.1, 0.2, 0.3, 0.4)
  x1b <- c()
  x2 <- c(0.4, 0.3, 0.2, 0.1)
  x3 <- c(0.2, 0.3, 0.4, 0.1, NA)
  x4 <- c(0.2, 0.3, 0.4, -0.1)
  x8 <- c(-0.2, -0.3, -0.4, -0.100001)
  xch <- c("a", "b", "c", "d", "e", "f", "g", "h")

#' @srrstats {G5.3} Expected to return objects containing no missing (`NA`)
  expect_true(any(is.na(x3)), all(!is.na(x3)))

  expect_vector(x, ptype = numeric(), size = 4)
  expect_equal(concstats_inequ(x, type = "entropy"),
               concstats_entropy(x, unbiased = FALSE))
  expect_equal(concstats_inequ(x, unbiased = TRUE, type = "entropy"),
               concstats_entropy(x, unbiased = TRUE))
  expect_equal(concstats_inequ(x1, type = "gini"), concstats_gini(x1))
  expect_equal(concstats_inequ(x1, unbiased = TRUE, type = "gini"),
               concstats_gini(x1, unbiased = TRUE))
  expect_equal(concstats_inequ(x, type = "simpson"), concstats_simpson(x))
  expect_equal(concstats_inequ(x, unbiased = TRUE, type = "simpson"),
               concstats_simpson(x, unbiased = TRUE))
  expect_equal(concstats_inequ(x1, type = "palma"), concstats_palma(x1))
  expect_equal(concstats_inequ(x2, type = "grs"), concstats_grs(x2))
  expect_equal(concstats_inequ(x, type = "Entropy"), concstats_entropy(x,
                                                            unbiased = FALSE))
  expect_equal(concstats_inequ(x, type = "all"), concstats_all_inequ(x))
  expect_error(concstats_inequ(x1b, na.rm = TRUE))
})

## concstats_entropy

test_that("concstats_entropy function operates properly", {
#' @srrstats {G5.1} Data used to test, made generally available and run examples.
  x <- c(0.2, 0.3, 0.4, 0.1)
  x1 <- c(0.2, 0.3, 0.4, -0.1)
  x1b <- c()
  x2 <- c(0.2, 0.3, 0.4, 0.1, NA)
  x3 <- c(0.4, 0.3, 0.2, 0.1)
  x4 <- c(20, 30, 40, 10)
  x5 <- c(0.2, 0.3, 0.4, 0.101)
  x6 <- c(0.2, 0.3, 0.4, 0.1, 0.0001)
  x8 <- c(-0.2, -0.3, -0.4, -0.100001)
  x9 <- c(NA, NA, NA, NA, NA, NA, NA, NA)
  xch <- c("a", "b", "c", "d", "e", "f", "g", "h")
  na.rm <- as.logical(TRUE | FALSE)
  unbiased <- as.logical(TRUE | FALSE)

#' @srrstats {G5.3} Expected to return objects containing no missing (`NA`)
  expect_true(any(is.na(x2)), all(!is.na(x2)))

  expect_true(all(round(x) == 0), (abs(x) > 0 & abs(x) <= 1))
  expect_vector(x, ptype = numeric(), size = 4)
  expect_equal(x, as.numeric(x4 / sum(x4)))
  expect_equal(concstats_entropy(x2, na.rm = FALSE), NA_real_)
#' @srrstats {G5.2, G5.2a, G5.2b, G5.8, G5.8a, G5.8b} Edge test for data of
#'  unsupported types
  expect_error(concstats_entropy(xch, !isTRUE(is.numeric(xch),
  "x in `concstats_entropy` must be a numeric vector\n",
  "You have provided an object of class: ", class(x)[1])))
#' @srrstats {G5.2, G5.2a, G5.2b, G5.8c} Error on vector with all-`NA` fields
  expect_error(concstats_entropy(x9, na.rm = TRUE))
  expect_error(concstats_entropy(x8, na.rm = TRUE))
  expect_error(concstats_entropy(x1b, na.rm = TRUE))
  expect_warning(concstats_entropy(x2, na.rm = 0))
  expect_warning(concstats_entropy(x2, unbiased = 0))
#' @srrstats {G3.0, EA6.0, EA6.0e} Return values, single-valued objects.
  expect_equal(concstats_entropy(x, isTRUE(all.equal(1, sum(x),
                                       tolerance = .Machine$double.eps^0.25))),
               concstats_entropy(x))
#' @srrstats {G3.0, G5.9, G5.9a} Adding trivial noise
  expect_equal(concstats_entropy(x6, isTRUE(all.equal(1, sum(x),
                                       tolerance = .Machine$double.eps^0.25))),
               concstats_entropy(x6))
#' @srrstats {G5.2, G5.2a, G5.2b, EA6.0, EA6.0e} Return values, single-valued
#'  objects
  expect_error(concstats_entropy(x1, !isTRUE(all.equal(1, sum(x1),
                                       tolerance = .Machine$double.eps^0.25))),
               "vector x in `concstats_entropy` does not sum to 1")
  expect_length(na.rm, 1L)
  expect_type(na.rm, "logical")
  expect_length(unbiased, 1L)
  expect_type(unbiased, "logical")

})

test_that("concstats_entropy returns the unbiased entropy measure ", {

#' @srrstats {G5.5, G5.4, G5.4a, G5.4c} Stored values (share_2018_) and vector
#'  are drawn from [this paper](https://doi.org/10.1515/zfgg-2022-0002),
#'   page 26/27

  x <- c(0.2, 0.3, 0.4, 0.1)
  share_2018_ent <- 0.8024276
  share_2018 <- c(0.012663407,0.029367501,0.014456455,0.012046011,0.007477799,
                  0.189784408,0.008738591,0.015635544,0.012787201,
                  0.013071539,0.046268385, 0.006580823, 0.009102, 0.00760554,
                  0.047173998, 0.034356881, 0.137813902, 0.016876624,
                  0.065780114, 0.053775553, 0.228519883, 0.030117841)

#' @srrstats {G3.0, EA6.0, EA6.0e} Return values, single-valued objects.
  expect_equal(concstats_entropy(share_2018), share_2018_ent,
               tolerance = .Machine$double.eps^0.25)
  expect_equal(concstats_entropy(x),(sum(-x / sum(x) * log(x / sum(x), base = 2))
                                     / log(sum(x > 0), base = 2)))
#' @srrstats {EA6.0, EA6.0a} Return values
  expect_true(is.numeric(share_2018_ent),
                         label = "numeric values returned")
})

test_that("concstats_entropy returns the biased entropy measure ", {

  x <- c(0.2, 0.3, 0.4, 0.1)
  share_2018_ent2 <- 3.578371
  share_2018 <- c(0.012663407,0.029367501,0.014456455,0.012046011,0.007477799,
                  0.189784408,0.008738591,0.015635544,0.012787201,
                  0.013071539,0.046268385, 0.006580823, 0.009102, 0.00760554,
                  0.047173998, 0.034356881, 0.137813902, 0.016876624,
                  0.065780114, 0.053775553, 0.228519883, 0.030117841)

#' @srrstats {G3.0, EA6.0, EA6.0e} Return values, single-valued objects.
  expect_equal(concstats_entropy(share_2018, unbiased = FALSE), share_2018_ent2,
               tolerance = .Machine$double.eps^0.25)
  expect_equal(concstats_entropy(x, unbiased = FALSE),
               sum(-x / sum(x) * log(x / sum(x), base = 2)))
#' @srrstats {EA6.0, EA6.0a} Return values
  expect_true(is.numeric(share_2018_ent2),
                         label = "numeric values returned")
})

## concstats_gini

test_that("concstats_gini function operates properly", {
#' @srrstats {G5.1} Data used to test, made generally available and run examples.
  x <- c(0.2, 0.3, 0.4, 0.1)
  x1 <- c(0.2, 0.3, 0.4, -0.1)
  x1b <- c()
  x2 <- c(0.2, 0.3, 0.4, 0.1, NA)
  x3 <- c(0.1, 0.2, 0.3, 0.4)
  x4 <- c(20, 30, 40, 10)
  x5 <- c(0.2, 0.3, 0.4, 0.101)
  x6 <- c(0.2, 0.3, 0.4, 0.1, 0.0001)
  x8 <- c(-0.2, -0.3, -0.4, -0.100001)
  x9 <- c(NA, NA, NA, NA, NA, NA, NA, NA)
  xch <- c("a", "b", "c", "d", "e", "f", "g", "h")
  na.rm <- as.logical(TRUE | FALSE)
  unbiased <- as.logical(TRUE | FALSE)

  expect_true(any(is.na(x2)), all(!is.na(x2)))
  expect_true(all(round(x) == 0), (abs(x) > 0 & abs(x) <= 1))
  expect_vector(x, ptype = numeric(), size = 4)
  expect_equal(x, as.numeric(x4 / sum(x4)))
  expect_equal(concstats_gini(x2, na.rm = FALSE), NA_real_)
  expect_equal(sort(x), x3)
#' @srrstats {G5.2, G5.2a, G5.2b, G5.8, G5.8b} Edge test for data of unsupported
#'  types
  expect_error(concstats_gini(xch, !isTRUE(is.numeric(xch),
  "x in `concstats_gini` must be a numeric vector\n",
  "You have provided an object of class: ", class(x)[1])))
#' @srrstats {G5.2, G5.2a, G5.2b, G5.8c} Error on vector with all-`NA` fields
  expect_error(concstats_gini(x9, na.rm = TRUE))
  expect_error(concstats_gini(x8, na.rm = TRUE))
  expect_error(concstats_gini(x1b, na.rm = TRUE))
  expect_warning(concstats_gini(x, na.rm = 0))
  expect_warning(concstats_gini(x, unbiased = 0))
#' @srrstats {G3.0, EA6.0, EA6.0e} Return values, single-valued objects.
  expect_error(concstats_gini(x1, !isTRUE(all.equal(1, sum(x1),
                                       tolerance = .Machine$double.eps^0.25))),
               "vector x in `concstats_gini` does not sum to 1")
  expect_length(na.rm, 1L)
  expect_type(na.rm, "logical")
  expect_length(unbiased, 1L)
  expect_type(unbiased, "logical")

})

test_that("concstats_gini returns the gini measure", {

  x <- c(0.1, 0.2, 0.3, 0.4)
  share_2018_gini <- 0.5793415
  share_2018 <- c(0.012663407,0.029367501,0.014456455,0.012046011,0.007477799,
                  0.189784408,0.008738591,0.015635544,0.012787201,
                  0.013071539,0.046268385, 0.006580823, 0.009102, 0.00760554,
                  0.047173998, 0.034356881, 0.137813902, 0.016876624,
                  0.065780114, 0.053775553, 0.228519883, 0.030117841)

#' @srrstats {G3.0, EA6.0, EA6.0e} Return values, single-valued objects.
  expect_equal(concstats_gini(share_2018), share_2018_gini,
               tolerance = .Machine$double.eps^0.25)
  expect_equal(concstats_gini(x), 2 * sum(x * seq_len(length(x))
                                / length(x) * sum(x)) - 1 - (1 / length(x)))
#' @srrstats {EA6.0, EA6.0a} Return values
  expect_true(is.numeric(share_2018_gini), label = "numeric values returned")
})

test_that("concstats_gini returns the unbiased gini measure", {

  x <- c(0.1, 0.2, 0.3, 0.4)
  share_2018_gini2 <- 0.6069292
  share_2018 <- c(0.012663407,0.029367501,0.014456455,0.012046011,0.007477799,
                  0.189784408,0.008738591,0.015635544,0.012787201,
                  0.013071539,0.046268385, 0.006580823, 0.009102, 0.00760554,
                  0.047173998, 0.034356881, 0.137813902, 0.016876624,
                  0.065780114, 0.053775553, 0.228519883, 0.030117841)

#' @srrstats {G3.0, EA6.0, EA6.0e} Return values, single-valued objects.
  expect_equal(concstats_gini(share_2018, unbiased = TRUE), share_2018_gini2,
               tolerance = .Machine$double.eps^0.25)
  expect_equal(concstats_gini(x, unbiased = TRUE), length(x) / (length(x) - 1) *
                 (2 * sum(x * seq_len(length(x))
                                / length(x) * sum(x)) - 1 - (1 / length(x))))
#' @srrstats {EA6.0, EA6.0a} Return values
  expect_true(is.numeric(share_2018_gini2), label = "numeric values returned")
})

## concstats_simpson

test_that("concstats_simpson function operates properly", {
#' @srrstats {G5.1} Data used to test, made generally available and run examples.
  x <- c(0.2, 0.3, 0.4, 0.1)
  x1 <- c(0.2, 0.3, 0.4, -0.1)
  x1b <- c()
  x2 <- c(0.2, 0.3, 0.4, 0.1, NA)
  x3 <- c(0.4, 0.3, 0.2, 0.1)
  x4 <- c(20, 30, 40, 10)
  x5 <- c(0.2, 0.3, 0.4, 0.101)
  x6 <- c(0.2, 0.3, 0.4, 0.1, 0.0001)
  x8 <- c(-0.2, -0.3, -0.4, -0.100001)
  x9 <- c(NA, NA, NA, NA, NA, NA, NA, NA)
  xch <- c("a", "b", "c", "d", "e", "f", "g", "h")
  na.rm <- as.logical(TRUE, FALSE)
  unbiased <- as.logical(TRUE, FALSE)

  expect_true(any(is.na(x2)), all(!is.na(x2)))
  expect_true(all(round(x) == 0), (abs(x) > 0 & abs(x) <= 1))
  expect_vector(x, ptype = numeric(), size = 4)
  expect_equal(x, as.numeric(x4 / sum(x4)))
  expect_equal(concstats_simpson(x2, na.rm = FALSE), NA_real_)
#' @srrstats {G5.2, G5.2a, G5.2b, G5.8, G5.8b} Edge test for data of unsupported
#'  types
  expect_error(concstats_simpson(xch, !isTRUE(is.numeric(xch),
  "x in `concstats_simpson` must be a numeric vector\n",
  "You have provided an object of class: ", class(x)[1] )))
#' @srrstats {G5.2, G5.2a, G5.2b, G5.8c} Error on vector with all-`NA` fields
  expect_error(concstats_simpson(x9, na.rm = TRUE))
  expect_error(concstats_simpson(x8, na.rm = TRUE))
  expect_error(concstats_simpson(x1b, na.rm = TRUE))
  expect_warning(concstats_simpson(x, na.rm = 0))
  expect_warning(concstats_simpson(x, unbiased = 0))
#' @srrstats {G3.0, EA6.0, EA6.0e} Return values, single-valued objects.
  expect_equal(concstats_simpson(x, isTRUE(all.equal(1, sum(x),
                                        tolerance = .Machine$double.eps^0.25))),
               concstats_simpson(x))
#' @srrstats {G3.0, G5.9, G5.9a} Adding trivial noise
  expect_equal(concstats_simpson(x6, isTRUE(all.equal(1, sum(x),
                                       tolerance = .Machine$double.eps^0.25))),
               concstats_simpson(x6))
#' @srrstats {G5.2, G5.2a, G5.2b, EA6.0, EA6.0e} Return values, single-valued
#'  objects
  expect_error(concstats_simpson(x1, !isTRUE(all.equal(1, sum(x1),
                                       tolerance = .Machine$double.eps^0.25))),
               "vector x in `concstats_simpson` does not sum to 1")
  expect_length(na.rm, 1L)
  expect_type(na.rm, "logical")
  expect_length(unbiased, 1L)
  expect_type(unbiased, "logical")

})

test_that("concstats_simpson returns the Simpson measure", {

  x <- c(0.2, 0.3, 0.4, 0.1)
  share_2018_sim <- 0.8765386
  share_2018 <- c(0.012663407,0.029367501,0.014456455,0.012046011,0.007477799,
                  0.189784408,0.008738591,0.015635544,0.012787201,
                  0.013071539,0.046268385, 0.006580823, 0.009102, 0.00760554,
                  0.047173998, 0.034356881, 0.137813902, 0.016876624,
                  0.065780114, 0.053775553, 0.228519883, 0.030117841)

#' @srrstats {G3.0, EA6.0, EA6.0e} Return values, single-valued objects.
  expect_equal(concstats_simpson(share_2018), share_2018_sim,
               tolerance = .Machine$double.eps^0.25)
  expect_equal(concstats_simpson(x), 1 - sum(x ^ 2))
#' @srrstats {EA6.0, EA6.0a} Return values
  expect_true(is.numeric(share_2018_sim),
                         label = "numeric values returned")
})

test_that("concstats_simpson returns the unbiased simpson measure", {

  x <- c(0.1, 0.2, 0.3, 0.4)
  share_2018_sim2 <- 0.8765386
  share_2018 <- c(0.012663407,0.029367501,0.014456455,0.012046011,0.007477799,
                  0.189784408,0.008738591,0.015635544,0.012787201,
                  0.013071539,0.046268385, 0.006580823, 0.009102, 0.00760554,
                  0.047173998, 0.034356881, 0.137813902, 0.016876624,
                  0.065780114, 0.053775553, 0.228519883, 0.030117841)

#' @srrstats {G3.0, EA6.0, EA6.0e} Return values, single-valued objects.
  expect_equal(concstats_simpson(share_2018), share_2018_sim2,
               tolerance = .Machine$double.eps^0.25)
  expect_equal(concstats_simpson(x, unbiased = TRUE),
               as.numeric(1 - sum(x ^ 2) / (sum(x / sum(x))) ^ 2))
#' @srrstats {EA6.0, EA6.0a} Return values
  expect_true(is.numeric(share_2018_sim2),
                         label = "numeric values returned")
})
## concstats_palma

test_that("concstats_palma function operates properly", {
#' @srrstats {G5.1} Data used to test, made generally available and run examples.
  x <- c(0.2, 0.3, 0.4, 0.1)
  x1 <- c(0.2, 0.3, 0.4, -0.1)
  x1b <- c()
  x2 <- c(0.2, 0.3, 0.4, 0.1, NA)
  x3 <- c(0.1, 0.2, 0.3, 0.4)
  x4 <- c(20, 30, 40, 10)
  x5 <- c(0.2, 0.3, 0.4, 0.101)
  x6 <- c(0.2, 0.3, 0.4, 0.1, 0.0001)
  x8 <- c(-0.2, -0.3, -0.4, -0.100001)
  x9 <- c(NA, NA, NA, NA, NA, NA, NA, NA)
  xch <- c("a", "b", "c", "d", "e", "f", "g", "h")
  na.rm <- as.logical(TRUE, FALSE)

  expect_true(any(is.na(x2)), all(!is.na(x2)))
  expect_true(all(round(x) == 0), (abs(x) > 0 & abs(x) <= 1))
  expect_vector(x, ptype = numeric(), size = 4)
  expect_equal(x, as.numeric(x4 / sum(x4)))
  expect_equal(concstats_palma(x2, na.rm = FALSE), NA_real_)
  expect_equal(sort(x), x3)
#' @srrstats {G5.2, G5.2a, G5.2b, G5.8, G5.8b} Edge test for data of unsupported
#'  types
  expect_error(concstats_palma(xch, !isTRUE(is.numeric(xch),
  "x in `concstats_palma` must be a numeric vector\n",
  "You have provided an object of class: ", class(x)[1] )))
#' @srrstats {G5.2, G5.2a, G5.2b, G5.8c} Error on vector with all-`NA` fields
  expect_error(concstats_palma(x9, na.rm = TRUE))
  expect_error(concstats_palma(x8, na.rm = TRUE))
  expect_error(concstats_palma(x1b, na.rm = TRUE))
  expect_warning(concstats_palma(x, na.rm = 0))
#' @srrstats {G3.0, EA6.0, EA6.0e} Return values, single-valued objects.
  expect_equal(concstats_palma(x, isTRUE(all.equal(1, sum(x),
                                       tolerance = .Machine$double.eps^0.25))),
               concstats_palma(x))
#' @srrstats {G3.0, G5.9, G5.9a} *Adding trivial noise
  expect_equal(concstats_palma(x6, isTRUE(all.equal(1, sum(x),
                                       tolerance = .Machine$double.eps^0.25))),
               concstats_palma(x6))
#' @srrstats {G5.2, G5.2a, G5.2b, EA6.0, EA6.0e} Return values, single-valued
#'  objects
  expect_error(concstats_palma(x1, !isTRUE(all.equal(1, sum(x1),
                                       tolerance = .Machine$double.eps^0.25))),
               "vector x in `concstats_palma` does not sum to 1")
  expect_length(na.rm, 1L)
  expect_type(na.rm, "logical")

})

test_that("concstats_palma returns the alternative palma inequality measure", {

  x <- c(0.2, 0.3, 0.5)
  share_2018_palma <- 6.174089
  share_2018 <- c(0.012663407,0.029367501,0.014456455,0.012046011,0.007477799,
                  0.189784408,0.008738591,0.015635544,0.012787201,
                  0.013071539,0.046268385, 0.006580823, 0.009102, 0.00760554,
                  0.047173998, 0.034356881, 0.137813902, 0.016876624,
                  0.065780114, 0.053775553, 0.228519883, 0.030117841)

#' @srrstats {G3.0, EA6.0, EA6.0e} Return values, single-valued objects.
  expect_equal(concstats_palma(share_2018), share_2018_palma,
               tolerance = .Machine$double.eps^0.25)

  expect_equal(concstats_palma(x),
               sum(x
                   [cut(x, stats::quantile(x, probs = seq(0, 1, 0.1)),
                        include.lowest = TRUE, labels = FALSE) > 9]) /
                 sum(x
                     [cut(x, stats::quantile(x, probs = seq(0, 1, 0.1)),
                          include.lowest = TRUE, labels = FALSE) <= 4]))
#' @srrstats {EA6.0, EA6.0a} Return values
  expect_true(is.numeric(share_2018_palma), label = "numeric values returned")
})

## concstats_grs

test_that("concstats_grs function operates properly", {
#' @srrstats {G5.1} Data used to test, made generally available and run examples.
  x <- c(0.2, 0.3, 0.4, 0.1)
  x1 <- c(0.2, 0.3, 0.4, -0.1)
  x1b <- c()
  x2 <- c(0.2, 0.3, 0.4, 0.1, NA)
  x3 <- c(0.4, 0.3, 0.2, 0.1)
  x4 <- c(20, 30, 40, 10)
  x5 <- c(0.2, 0.3, 0.4, 0.101)
  x6 <- c(0.2, 0.3, 0.4, 0.1, 0.0001)
  x8 <- c(-0.2, -0.3, -0.4, -0.100001)
  x9 <- c(NA, NA, NA, NA, NA, NA, NA, NA)
  xch <- c("a", "b", "c", "d", "e", "f", "g", "h")
  na.rm <- as.logical(TRUE | FALSE)

  expect_true(any(is.na(x2)), all(!is.na(x2)))
  expect_true(all(round(x) == 0), (abs(x) > 0 & abs(x) <= 1))
  expect_vector(x, ptype = numeric(), size = 4)
  expect_equal(x, as.numeric(x4 / sum(x4)))
  expect_equal(concstats_grs(x2, na.rm = FALSE), NA_real_)
  expect_equal(sort(x, decreasing = TRUE), x3)
#' @srrstats {G5.2, G5.2a, G5.2b, G5.8, G5.8b} Edge test for data of unsupported
#'  types
  expect_error(concstats_grs(xch, !isTRUE(is.numeric(xch),
  "x in `concstats_grs` must be a numeric vector\n",
  "You have provided an object of class: ", class(x)[1] )))
#' @srrstats {G5.2, G5.2a, G5.2b, G5.8c} Error on vector with all-`NA` fields
  expect_error(concstats_grs(x9, na.rm = TRUE))
  expect_error(concstats_grs(x8, na.rm = TRUE))
  expect_error(concstats_grs(x1b, na.rm = TRUE))
  expect_warning(concstats_grs(x, na.rm = 0))
#' @srrstats {G3.0, EA6.0, EA6.0e} Return values, single-valued objects.
  expect_equal(concstats_grs(x, isTRUE(all.equal(1, sum(x),
                                        tolerance = .Machine$double.eps^0.25))),
               concstats_grs(x))
#' @srrstats {G3.0, G5.9, G5.9a} Adding trivial noise
  expect_equal(concstats_grs(x6, isTRUE(all.equal(1, sum(x),
                                       tolerance = .Machine$double.eps^0.25))),
               concstats_grs(x6))
#' @srrstats {G5.2, G5.2a, G5.2b, EA6.0, EA6.0e} Return values, single-valued
#'  objects
  expect_error(concstats_grs(x1, !isTRUE(all.equal(1, sum(x1),
                                       tolerance = .Machine$double.eps^0.25))),
               "vector x in `concstats_grs` does not sum to 1")
  expect_length(na.rm, 1L)
  expect_type(na.rm, "logical")

})

test_that("concstats_grs returns the alternative grs measure", {

  x <- c(0.4, 0.3, 0.2, 0.1)
  share_2018_grs <- 0.2284457
  share_2018 <- c(0.012663407,0.029367501,0.014456455,0.012046011,0.007477799,
                  0.189784408,0.008738591,0.015635544,0.012787201,
                  0.013071539,0.046268385, 0.006580823, 0.009102, 0.00760554,
                  0.047173998, 0.034356881, 0.137813902, 0.016876624,
                  0.065780114, 0.053775553, 0.228519883, 0.030117841)

#' @srrstats {G3.0, EA6.0, EA6.0e} Return values, single-valued objects.
  expect_equal(concstats_grs(share_2018), share_2018_grs,
               tolerance = .Machine$double.eps^0.25)

  expect_equal(concstats_grs(x), sum((sum(x > 0) ^ 2 * x[1] + 0.3 * x ^ 2) /
                             (sum(x > 0) ^ 2 + sum(x > 0) * 0.3 * x[1] * x) *
                             x))
#' @srrstats {EA6.0, EA6.0a} Return values
  expect_true(is.numeric(share_2018_grs), label = "numeric values returned")
})

test_that("concstats_all_inequ returns a data frame", {
#' @srrstats {G5.1} Data used to test, made generally available and run examples.
  x <- c(0.2, 0.3, 0.4, 0.1)
  x1 <- c(0.2, 0.3, 0.4, -0.1)
  x1b <- c()
  x2 <- c(0.2, 0.3, 0.4, 0.1, NA)
  x4 <- c(20, 30, 40, 10)
  x5 <- c(0.2, 0.3, 0.4, 0.101)
  x6 <- c(0.2, 0.3, 0.4, 0.1, 0.0001)
  x8 <- c(-0.2, -0.3, -0.4, -0.100001)
  x9 <- c(NA, NA, NA, NA, NA, NA, NA, NA)
  xch <- c("a", "b", "c", "d", "e", "f", "g", "h")
  na.rm <- as.logical(TRUE | FALSE)
  dummy_df <- data.frame(Measure = rep(letters[1:5]), Value = c(1,2,3,4,5))

  expect_vector(x, ptype = numeric(), size = 4)
  expect_true(any(is.na(x2)), all(!is.na(x2)))
#' @srrstats {EA6.0, EA6.0a, EA6.0b, EA6.0c,  EA6.0d} Classes, dimensions, and
#'  types of objects
  expect_equal(ncol(dummy_df), 2)
  expect_equal(x, as.numeric(x4 / sum(x4)))
  expect_true(is.numeric(dummy_df$Value))
  expect_type(dummy_df$Value, "double")
  expect_type(dummy_df$Measure, "character")
  expect_identical(names(dummy_df), c("Measure", "Value"))
  expect_true(is.data.frame(concstats_all_inequ(x)), "data.frame")
  expect_equal(concstats_all_inequ(x2, na.rm = FALSE), NA_real_)
#' @srrstats {G5.2, G5.2a, G5.2b, G5.8, G5.8b} Edge test for data of unsupported
#'  types
  expect_error(concstats_all_inequ(xch, na.rm = TRUE,
                                   !isTRUE(as.numeric(xch),
                                   "x in `concstats_all_inequ` must be a
                                   numeric vector\n",
                                   "You have provided an object of class:",
                                   class(x)[1])))
#' @srrstats {G5.2, G5.2a, G5.2b, G5.8c} Error on vector with all-`NA` fields
  expect_error(concstats_all_inequ(x9, na.rm = TRUE))
  expect_error(concstats_all_inequ(x8, na.rm = TRUE))
  expect_error(concstats_all_inequ(x1b, na.rm = TRUE))
  expect_warning(concstats_all_inequ(x, na.rm = 0))
#' @srrstats {G3.0, G5.9, G5.9a} Adding trivial noise
  expect_equal(concstats_all_inequ(x6, isTRUE(all.equal(1, sum(x),
                                       tolerance = .Machine$double.eps^0.25))),
               concstats_all_inequ(x6))
  expect_equal(concstats_grs(x, isTRUE(all.equal(1, sum(x),
                                       tolerance = .Machine$double.eps^0.25))),
               concstats_grs(x))
#' @srrstats {G5.2, G5.2a, G5.2b, EA6.0, EA6.0e} Return values, single-valued
#'  objects
  expect_error(concstats_all_inequ(x1, !isTRUE(all.equal(1, sum(x1),
                                       tolerance = .Machine$double.eps^0.25))),
               "vector x in `concstats_all_inequ` does not sum to 1")
  expect_length(na.rm, 1L)
  expect_type(na.rm, "logical")


})
