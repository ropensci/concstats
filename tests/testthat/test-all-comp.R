local_edition(3)

## concstats_all_comp

test_that("concstats_all_comp returns a data frame", {
  #' @srrstats {G5.1} Data used to test, made generally available and run
  #'  examples.
  x <- c(0.2, 0.25, 0.4, 0.1, 0.05)
  x1 <- c(0.2, 0.3, 0.25, 0.05, -0.2)
  x1b <- c()
  x2 <- c(0.2, 0.3, 0.4, 0.1, NA)
  x3 <- c(0.4, 0.25, 0.2, 0.1, 0.05)
  x4 <- c(20, 25, 40, 10, 5)
  x5 <- c(0.2, 0.25, 0.4, 0.05, 0.10001)
  x7 <- c(0.2, 0.2499999999, 0.4, 0.1, 0.05)
  x8 <- c(-0.2, -0.3, -0.4, -0.100001)
  x9 <- c(NA, NA, NA, NA, NA)
  xch <- c("a", "b", "c", "d", "e")

  dummy_df <- data.frame(Measure = rep(letters[1:5]), Value = c(1, 2, 3, 4, 5))

  expect_vector(x, ptype = numeric(), size = 5)
  expect_true(any(is.na(x2)), all(!is.na(x2)))
  #' @srrstats {EA6.0, EA6.0a, EA6.0b, EA6.0c,  EA6.0d} Classes, dimensions, and
  #'  types of objects
  expect_equal(ncol(dummy_df), 2)
  expect_equal(x, as.numeric(x4 / sum(x4)))
  expect_true(is.numeric(dummy_df$Value))
  expect_type(dummy_df$Value, "double")
  expect_type(dummy_df$Measure, "character")
  expect_identical(names(dummy_df), c("Measure", "Value"))
  expect_true(is.data.frame(concstats_all_comp(x)), "data.frame")
  expect_equal(concstats_all_comp(x2, na.rm = FALSE), NA_real_)
  #' @srrstats {G5.2, G5.2a, G5.2b, G5.8, G5.8b} Edge test for data of
  #'  unsupported types
  expect_error(concstats_all_comp(xch, !is.numeric(xch)))
  #' @srrstats {G5.0, G5.2, G5.2a, G5.2b, G5.8c} Error and other messages/warnings
  #' on vector with all-`NA` fields
  expect_error(concstats_all_comp(x9, na.rm = TRUE))
  expect_error(concstats_all_comp(x8, na.rm = TRUE))
  expect_error(concstats_all_comp(x1b, na.rm = TRUE))
  expect_error(concstats_all_comp(x, na.rm = 0))
  #' @srrstats {G5.2, G5.2a, G5.2b, EA6.0, EA6.0e} Return values, single-valued
  #'  objects
  expect_error(concstats_all_comp(sum(x1), 1,
                                  tolerance = .Machine$double.eps^0.25))
  #' @srrstats {G3.0, EA6.0, EA6.0e} Return values single-valued objects.
  act <- concstats_all_comp(x)
  exp <- concstats_all_comp(x3)
  expect_equal(act, exp, tolerance = .Machine$double.eps^0.25)
  #' @srrstats {G3.0, G5.9, G5.9a} Adding trivial noise
  act <- concstats_all_comp(x)
  exp <- concstats_all_comp(x7)

})
