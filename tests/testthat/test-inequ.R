local_edition(3)

## concstats_inequ

test_that("concstats_inequ function operates / switches properly", {
  x <- c(0.2, 0.25, 0.4, 0.1, 0.05)
  x1 <- c(0.05, 0.1, 0.2, 0.25, 0.4)
  x1b <- c()
  x2 <- c(0.2, 0.3, 0.4, 0.1, NA)
  x3 <- c(0.4, 0.25, 0.2, 0.1, 0.05)
  x4 <- c(20, 25, 40, 10, 5)
  x5 <- c(0.2, 0.25, 0.4, 0.05, 0.10001)
  x7 <- c(0.2, 0.2499999999, 0.4, 0.1, 0.05)
  x8 <- c(-0.2, -0.3, -0.4, -0.100001)
  x9 <- c(NA, NA, NA, NA, NA)
  xch <- c("a", "b", "c", "d", "e")

  expect_true(any(is.na(x2)), all(!is.na(x2)))

  expect_vector(x, ptype = numeric(), size = 5)
  expect_equal(concstats_inequ(x, type = "entropy"),
               concstats_entropy(x, normalized = TRUE))
  expect_equal(concstats_inequ(x, normalized = TRUE, type = "entropy"),
               concstats_entropy(x, normalized = TRUE))
  expect_equal(concstats_inequ(x, normalized = TRUE, type = "Entropy"),
               concstats_entropy(x, normalized = TRUE))
  expect_equal(concstats_inequ(x1, type = "gini"),
               concstats_gini(x1, normalized = TRUE))
  expect_equal(concstats_inequ(x1, type = "Gini"),
               concstats_gini(x1, normalized = TRUE))
  expect_equal(concstats_inequ(x1, type = "GINI"),
               concstats_gini(x1, normalized = TRUE))
  expect_equal(concstats_inequ(x1, normalized = TRUE, type = "gini"),
               concstats_gini(x1))
  expect_equal(concstats_inequ(x, type = "simpson"), concstats_simpson(x))
  expect_equal(concstats_inequ(x1, type = "palma"), concstats_palma(x1))
  expect_equal(concstats_inequ(x, type = "grs"), concstats_grs(x))
  expect_equal(concstats_inequ(x, type = "Entropy"), concstats_entropy(x,
                                                            normalized = TRUE))
  expect_equal(concstats_inequ(x, type = "all"), concstats_all_inequ(x))
  expect_true(is.data.frame(concstats_inequ(x, type = "all")), "data.frame")
  expect_error(concstats_inequ(x1b, na.rm = TRUE))
  expect_error(concstats_inequ(x, na.rm = 0))
  expect_message(concstats_inequ(x2))
  expect_error(concstats_inequ(x, normalized = NA))
  # digits argument
  expect_error(expect_int(x, digits = c(8, 0)))
  # Adding trivial noise
  act <- concstats_inequ(x, type = "entropy")
  exp <- concstats_inequ(x5)
  expect_equal(act, exp, tolerance = .Machine$double.eps^0.25)


})


