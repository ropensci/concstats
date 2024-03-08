local_edition(3)

## concstats_comp

test_that("concstats_comp function operates / switches properly", {
#' @srrstats {G5.1} Data used to test, made generally available and run
#' examples.
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

  expect_vector(x, ptype = numeric(), size = 5)
  expect_equal(concstats_comp(x, type = "hhi"),
               concstats_hhi(x))
  expect_equal(concstats_comp(x, normalized = TRUE, type = "hhi"),
               concstats_hhi(x, normalized = TRUE))
  expect_equal(concstats_comp(x, type = "hhi_d"), concstats_hhi_d(x))
  expect_equal(concstats_comp(x, type = "hhi_min"), concstats_hhi_min(x))
  expect_equal(concstats_comp(x, type = "dom"), concstats_dom(x))
  expect_equal(concstats_comp(x, type = "sten"), concstats_sten(x))
  expect_equal(concstats_comp(x, type = "HHI"), concstats_hhi(x))
  expect_equal(concstats_comp(x, type = "ALL"), concstats_all_comp(x))
  expect_error(concstats_comp(x1b, na.rm = TRUE))

})


