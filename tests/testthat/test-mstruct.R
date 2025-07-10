local_edition(3)

## concstats_mstruct

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

test_that("concstats_mstruct function operates / switches properly", {
  expect_true(any(is.na(x2)), all(!is.na(x2)))
  expect_vector(x, ptype = numeric(), size = 5)
  expect_equal(concstats_mstruct(x, type = "firm"), concstats_firm(x))
  expect_equal(concstats_mstruct(x, type = "FIRM"), concstats_firm(x))
  expect_equal(concstats_mstruct(x, type = "nrs_eq"), concstats_nrs_eq(x))
  expect_equal(concstats_mstruct(x, type = "top"), concstats_top(x, na.rm = FALSE))
  expect_equal(concstats_mstruct(x, type = "Top"), concstats_top(x))
  expect_equal(concstats_mstruct(x, type = "top3"), concstats_top3(x))
  expect_equal(concstats_mstruct(x, type = "Top3"), concstats_top3(x))
  expect_equal(concstats_mstruct(x, type = "top5"), concstats_top5(x))
  expect_equal(concstats_mstruct(x, type = "Top5"), concstats_top5(x))
  expect_equal(concstats_mstruct(x, type = "Firm"), concstats_firm(x))
  expect_equal(concstats_mstruct(x, type = "FiRm"), concstats_firm(x))
  expect_equal(concstats_mstruct(x, type = "all"), concstats_all_mstruct(x))

  expect_error(concstats_mstruct(x1b, na.rm = TRUE))
  expect_error(concstats_mstruct(x, na.rm = 0))
  expect_message(concstats_mstruct(x2))
  # digits argument
  expect_error(expect_int(x, digits = c(8, 0)))
  # Adding trivial noise
  act <- concstats_mstruct(x, type = "firm")
  exp <- concstats_mstruct(x5)
  expect_equal(act, exp, tolerance = .Machine$double.eps^0.25)
})


test_that("concstats_mstruct returns a data frame if type=all",{
  expect_true(is.data.frame(concstats_mstruct(x, type = "all")), "data.frame")
})
