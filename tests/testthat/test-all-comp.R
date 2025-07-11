local_edition(3)

## concstats_all_comp

test_that("concstats_all_comp returns a data frame", {
  x <- c(0.2, 0.25, 0.4, 0.1, 0.05)
  x1 <- c(0.2, 0.3, 0.25, 0.05, -0.2)
  x1b <- c()
  x2 <- c(0.2, 0.3, 0.4, 0.1, NA)
  x3 <- c(0.4, 0.25, 0.2, 0.1, 0.05)
  x4 <- c(20, 25, 40, 10, 5)
  x5 <- c(0.2, 0.25, 0.4, 0.05, 0.1000000001)
  x7 <- c(0.2, 0.249, 0.4, 0.1, 0.05)
  x8 <- c(-0.2, -0.3, -0.4, -0.100001)
  x9 <- c(NA, NA, NA, NA, NA)
  xch <- c("a", "b", "c", "d", "e")

#' @srrstats {G5.2b} testing of message, error behaviour
#' @srrstats {EA6.0a} Classes and types of objects
#' @srrstats {EA6.0b} Dimensions of tabular objects
#' @srrstats {EA6.0c} Column names (or equivalent) of tabular objects
#' @srrstats {EA6.0d} Classes or types of all columns contained within
#'  `data.frame`-type tabular objects
#' @srrstats {G5.3, G5.8b, G5.8c} Testing for absence of NAs in return objects


  dummy_df <- data.frame(Measure = rep(letters[1:5]), Value = c(1, 2, 3, 4, 5))
  expect_equal(ncol(dummy_df), 2)
  expect_true(is.numeric(dummy_df$Value))
  expect_type(dummy_df$Value, "double")
  expect_type(dummy_df$Measure, "character")
  expect_identical(names(dummy_df), c("Measure", "Value"))

  expect_vector(x, ptype = numeric(), size = 5)
  expect_true(any(is.na(x2)), all(!is.na(x2)))
  expect_equal(x, as.numeric(x4 / sum(x4)))


  expect_true(is.data.frame(concstats_all_comp(x)), "data.frame")
  expect_error(concstats_all_comp(xch, !is.numeric(xch)))
  expect_error(concstats_all_comp(x9, na.rm = TRUE))
  expect_error(concstats_all_comp(x8, na.rm = TRUE))
  expect_error(concstats_all_comp(x1b, na.rm = TRUE))
  expect_error(concstats_all_comp(x, na.rm = 0))
  expect_error(concstats_all_comp(x, normalized = 0))

  # digits argument
  expect_error(expect_int(x, digits = c(8, 0)))

#' @srrstats {EA6.0, EA6.0e, EA5.8} Values of single-valued objects; for `numeric` values
#'  either using `testthat::expect_equal()` or equivalent with a defined value
#'  for the `tolerance` parameter
  # test that sum of x=1
  expect_error(concstats_all_comp(sum(x1), 1,
                                  tolerance = .Machine$double.eps^0.25))

  # test if order of values matter (should not)
  act <- concstats_all_comp(as.numeric(x))
  exp <- concstats_all_comp(as.numeric(x3))
  expect_equal(act, exp, tolerance = .Machine$double.eps^0.25)
  # convert to decimal
  act <- concstats_all_comp(as.numeric(x))
  exp <- concstats_all_comp(as.numeric(x4 / sum(x4)))
  expect_equal(act, exp, tolerance = .Machine$double.eps^0.25)
  # adding trivial noise
  act <- concstats_all_comp(as.numeric(x))
  exp <- concstats_all_comp(as.numeric(x5))
  expect_equal(act, exp, tolerance = .Machine$double.eps^0.25)

})

