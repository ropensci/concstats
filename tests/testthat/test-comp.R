local_edition(3)

## concstats_comp

test_that("concstats_comp function operates / switches properly", {
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

  expect_vector(x, ptype = numeric(), size = 5)
  expect_equal(concstats_comp(x, type = "hhi"),
               concstats_hhi(x))
  expect_equal(concstats_comp(x, type = "Hhi"),
               concstats_hhi(x))
  expect_equal(concstats_comp(x, normalized = TRUE, type = "hhi"),
               concstats_hhi(x, normalized = TRUE))
  expect_equal(concstats_comp(x, type = "hhi_d"), concstats_hhi_d(x))
  expect_equal(concstats_comp(x, type = "hhi_min"), concstats_hhi_min(x))
  expect_equal(concstats_comp(x, type = "dom"), concstats_dom(x))
  expect_equal(concstats_comp(x, type = "Dom"), concstats_dom(x))
  expect_equal(concstats_comp(x, type = "sten"), concstats_sten(x))
  expect_equal(concstats_comp(x, type = "HHI"), concstats_hhi(x))
  expect_equal(concstats_comp(x, type = "ALL"), concstats_all_comp(x))
  expect_equal(concstats_comp(x, type = "all"), concstats_all_comp(x))
  expect_true(is.data.frame(concstats_comp(x, type = "all")), "data.frame")
  expect_error(concstats_comp(x1b, na.rm = TRUE))
  expect_error(concstats_comp(x, na.rm = 0))
  expect_message(concstats_comp(x2))
  # digits argument
  expect_error(expect_int(x, digits = c(8, 0)))

  # Adding trivial noise
  act <- concstats_comp(x, type = "hhi")
  exp <- concstats_comp(x5)
  expect_equal(act, exp, tolerance = .Machine$double.eps^0.25)
})


