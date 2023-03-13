#' @title Market Structure Measures
#'
#' @srrstats {G1.4} roxygen2 used to document functions
#'
#' @description Set of different market structure measures to reflect a given
#'  market structure.
#'
#' @usage
#'  concstats_mstruct(x, type = c("firm", "nrs_eq", "top", "top3", "top5",
#'  "all"), na.rm = TRUE, digits = NULL)
#' @srrstats {G2.0a, G2.1a, EA1.1, EA1.3} accepted as input, length and type
#' @param x A non-negative numeric vector.
#' @param type A character string of the measure to be calculated,
#'  can be abbreviated with the first letter. Defaults to "firm". Input is not
#'  case-sensitive.
#' @param na.rm A logical vector that indicates whether \code{NA} values should
#'  be excluded or not. Must be either \code{TRUE} or \code{FALSE}. The default
#'  is \code{TRUE}. If set to \code{FALSE} the computation yields \code{NA} if
#'  vector contains \code{NA} values.
#' @param digits A non-null value for digits specifies the minimum number of
#'  significant digits to be printed in values. The default is \code{NULL} and
#'  will use base R print option. Significant digits defaults to 7.
#' @details
#'  \code{concstats_mstruct} is a wrapper for the proposed structural measures
#'  [concstats_firm()], returns the number of firms with a given market share
#'  [concstats_nrs_eq()] computes the reciprocal of the HHI, which indicates
#'  the equivalent number of firms of the same size,
#'  [concstats_top()], [concstats_top3()], and [concstats_top5()]
#'  calculate the share of the top (top 3 and top 5) firm(s) and returns the
#'  value in percentage. [concstats_all_mstruct()] computes all measures in
#'  a one step procedure. All measures can be computed individually.
#'
#' @return A single calculated numeric measure or `data frame`.
#' @note The vector of market shares should be in a decimal form corresponding
#'  to total share of individual firms/units.The sum of the vector should sum up
#'  to 1.
#'
#' @seealso [concstats_concstats()],[concstats_comp()],[concstats_inequ()]
#'
#' @examples
#' # a vector of market shares
#' x <- c(0.35, 0.4, 0.05, 0.1, 0.06, 0.04)
#' # the number of firms with market share
#' concstats_mstruct(x, type = "firm")
#' # Calculate top market share individually
#' concstats_top(x)
#' # Calculate the market structure group measures
#' concstats_mstruct(x, type = "all", digits = 2)
#'
#' @export concstats_mstruct
concstats_mstruct <- function(x,
                    type = c("firm", "nrs_eq", "top", "top3", "top5", "all"),
                    na.rm = TRUE, digits = NULL) {
  type <- tolower(as.character(type))
#' @srrstats {G2.4, G2.4c} explicit conversion to character via as.character()
#' @srrstats {G2.3, G2.3b, G2.4c} used `tolower()` on line#50

#' @srrstats {G2.3, G2.3a} Used `match.arg()`
    switch(match.arg(type),
         firm = concstats_firm(x, na.rm = na.rm),
         nrs_eq = concstats_nrs_eq(x, na.rm = na.rm),
         top = concstats_top(x, na.rm = na.rm),
         top3 = concstats_top3(x, na.rm = na.rm),
         top5 = concstats_top5(x, na.rm = na.rm),
         all = concstats_all_mstruct(x, na.rm = na.rm, digits = digits))
}

#' @export
#' @title Number of firms
#' @rdname concstats_firm
#' @param x A non-negative numeric vector.
#' @param na.rm Logical vector that indicates whether \code{NA} values should
#'  be excluded or not. Must be either \code{TRUE} or \code{FALSE}. The default
#'  is \code{TRUE}.
#'  If set to \code{FALSE} the computation yields \code{NA} if the vector
#'  contains \code{NA} values.
#'@return A positive numeric integer.
#' @examples
#' # a vector of market shares
#' x <- c(0.4, 0.2, 0.25, 0.1, 0.05)
#' concstats_firm(x)
concstats_firm <- function(x, na.rm = TRUE) {
#' @srrstats {G5.8a} Zero-length data
#' @srrstats {G2.2, G2.6, G2.16} Checking class, type, NaN handling
  if (!is.numeric(x)) {
    stop("`x` in concstats_firm must be a numeric vector\n",
         "You have provided an object of class:", class(x)[1])
  }
#' @srrstats {G2.0, G2.1}
  if (!is.logical(na.rm) || !length(na.rm) == 1 || is.na(na.rm)) {
    stop("`na.rm` in `concstats_firm` must be either TRUE or FALSE")
  }
#' @srrstats {G2.13, G2.14, G2.14a, G2.14b, G2.15} Handling of missing values
  if (na.rm == TRUE) {
    x <- as.numeric(x[!is.na(x)])
  }

  if (!na.rm && any(is.na(x))) return(NA_real_)

  # check if x is a positive decimal vector
  if (as.logical(all(x < 0))) {
    stop("x in `concstats_firm` must be a positive vector")
  }
#' @srrstats {G3.0, EA6.0, EA6.0e} Return values, single-valued objects.
  # check sum of vector. Must sum to 1
  if (!isTRUE(all.equal(sum(x), 1, tolerance = .Machine$double.eps^0.25))) {
    stop("vector `x` in `concstats_firm` does not sum to 1")
  }

#' @srrstats {G2.4, G2.4b} explicit conversion to continuous via `as.numeric()`
  if (sum(x, na.rm = TRUE) > 1) {
    x <-  as.numeric(x / sum(x, na.rm = TRUE))
  }

  x <- as.numeric(stats::na.omit(x))
  firm <- as.numeric(sum(x > 0))
  return(firm)
}

#' @export
#' @title Numbers equivalent
#' @rdname concstats_nrs_eq
#' @param x A non-negative numeric vector.
#' @param na.rm A logical vector that indicates whether \code{NA} values should
#'  be excluded or not. Must be either \code{TRUE} or \code{FALSE}. The default
#'  is \code{TRUE}.
#'  If set to \code{FALSE} the computation yields \code{NA} if \code{NA} values
#'  are present.
#'@return A positive numeric value.
#' @examples
#' # a vector of market shares
#' x <- c(0.4, 0.2, 0.25, 0.1, 0.05)
#' concstats_nrs_eq(x)
concstats_nrs_eq <- function(x, na.rm = TRUE) {
#' @srrstats {G5.8a} Zero-length data
#' @srrstats {G2.2, G2.6, G2.16} Checking class, type, NaN handling
  if (!is.numeric(x)) {
    stop("`x` in concstats_nrs_eq must be a numeric vector\n",
         "You have provided an object of class:", class(x)[1])
  }
#' @srrstats {G2.0, G2.1}
  if (!is.logical(na.rm) || !length(na.rm) == 1 || is.na(na.rm)) {
    stop("`na.rm` in `concstats_nrs_eq` must be either TRUE or FALSE")
  }
#' @srrstats {G2.13, G2.14, G2.14a, G2.14b, G2.15} Handling of missing values
  if (na.rm == TRUE) {
    x <- as.numeric(x[!is.na(x)])
  }

  if (!na.rm && any(is.na(x))) return(NA_real_)

  # check if x is a positive decimal vector
  if (as.logical(all(x < 0))) {
    stop("x in `concstats_nrs_eq` must be a positive vector")
  }
#' @srrstats {G3.0, EA6.0, EA6.0e} Return values, single-valued objects.
  # check sum of vector. Must sum to 1
  if (!isTRUE(all.equal(sum(x), 1, tolerance = .Machine$double.eps^0.25))) {
    stop("vector `x` in `concstats_nrs_eq` does not sum to 1")
  }

#' @srrstats {G2.4, G2.4b} explicit conversion to continuous via `as.numeric()`
  if (sum(x, na.rm = TRUE) > 1) {
    x <-  as.numeric(x / sum(x, na.rm = TRUE))
  } else {
    x
  }

  x <- as.numeric(stats::na.omit(x))
  nrs_eq <- as.numeric(1 / sum(x ^ 2))
  return(nrs_eq)
}

#' @export
#' @title Top market share
#' @rdname concstats_top
#' @param x A non-negative numeric vector.
#' @param na.rm A logical vector that indicates whether \code{NA} values should
#'  be excluded or not. Must be either \code{TRUE} or \code{FALSE}. The default
#'  is \code{TRUE}.
#'  If set to \code{FALSE} the computation yields \code{NA} if \code{NA} values
#'  are present.
#' @return A positive numeric value, which indicates the top market share in
#'  percent.
#' @examples
#' # a vector of market shares
#' x <- c(0.4, 0.2, 0.25, 0.1, 0.05)
#' concstats_top(x)
concstats_top <- function(x, na.rm = TRUE) {
#' @srrstats {G5.8a} Zero-length data
#' @srrstats {G2.2, G2.6, G2.16} Checking class, type, NaN handling
  if (!is.numeric(x)) {
    stop("`x` in concstats_top must be a numeric vector\n",
         "You have provided an object of class:", class(x)[1])
  }
#' @srrstats {G2.0, G2.1}
  if (!is.logical(na.rm) || !length(na.rm) == 1 || is.na(na.rm)) {
    stop("`na.rm` in `concstats_top` must be either TRUE or FALSE")
  }
#' @srrstats {G2.13, G2.14, G2.14a, G2.14b, G2.15} Handling of missing values
  if (na.rm == TRUE) {
    x <- as.numeric(x[!is.na(x)])
  }

  if (!na.rm && any(is.na(x))) return(NA_real_)

  # check if x is a positive decimal vector
  if (as.logical(all(x < 0))) {
    stop("x in `concstats_top` must be a positive vector")
  }
#' @srrstats {G3.0, EA6.0, EA6.0e} Return values, single-valued objects.
  # check sum of vector. Must sum to 1
  if (!isTRUE(all.equal(sum(x), 1, tolerance = .Machine$double.eps^0.25))) {
    stop("vector `x` in `concstats_top` does not sum to 1")
  }

#' @srrstats {G2.4, G2.4b} explicit conversion to continuous via `as.numeric()`
  if (sum(x, na.rm = TRUE) > 1) {
    x <-  as.numeric(x / sum(x, na.rm = TRUE))
  } else {
    x
  }

  x <- sort(x, decreasing = TRUE)
  top <- as.numeric(x[1] * 100)
  return(top)
}

#' @export
#' @title Top 3 market share
#' @rdname concstats_top3
#' @param x A non-negative numeric vector.
#' @param na.rm A logical vector that indicates whether \code{NA} values should
#'  be excluded or not. Must be either \code{TRUE} or \code{FALSE}. The default
#'  is \code{TRUE}.
#'  If set to \code{FALSE} the computation yields \code{NA} if the vector
#'  contains \code{NA} values.
#' @return A positive numeric value, which indicates the sum of the top 3
#'  market shares as a percentage.
#' @examples
#' # a vector of market shares
#' x <- c(0.4, 0.2, 0.25, 0.1, 0.05)
#' concstats_top3(x)
concstats_top3 <- function(x, na.rm = TRUE) {
#' @srrstats {G5.8a} Zero-length data
#' @srrstats {G2.2, G2.6, G2.16} Checking class, type, NaN handling
  if (!is.numeric(x)) {
    stop("`x` in concstats_top3 must be a numeric vector\n",
         "You have provided an object of class:", class(x)[1])
  }
#' @srrstats {G2.0, G2.1}
  if (!is.logical(na.rm) || !length(na.rm) == 1 || is.na(na.rm)) {
    stop("`na.rm` in `concstats_top3` must be either TRUE or FALSE")
  }
#' @srrstats {G2.13, G2.14, G2.14a, G2.14b, G2.15} Handling of missing values
  if (na.rm == TRUE) {
    x <- as.numeric(x[!is.na(x)])
  }

  if (!na.rm && any(is.na(x))) return(NA_real_)

  # check if x is a positive decimal vector
  if (as.logical(all(x < 0))) {
    stop("x in `concstats_top3` must be a positive vector")
  }
#' @srrstats {G3.0, EA6.0, EA6.0e} Return values, single-valued objects.
  # check sum of vector. Must sum to 1
  if (!isTRUE(all.equal(sum(x), 1, tolerance = .Machine$double.eps^0.25))) {
    stop("vector `x` in `concstats_top3` does not sum to 1")
  }

#' @srrstats {G2.4, G2.4b} explicit conversion to continuous via `as.numeric()`
  if (sum(x, na.rm = TRUE) > 1) {
    x <-  as.numeric(x / sum(x, na.rm = TRUE))
  } else {
    x
  }

  x <- sort(x, decreasing = TRUE)
  top3 <- as.numeric(sum(x[1:3], na.rm = TRUE) * 100)
  return(top3)
}

#' @export
#' @title Top 5 market share
#' @rdname concstats_top5
#' @param x A non-negative numeric vector.
#' @param na.rm A logical vector that indicates whether \code{NA} values should
#'  be excluded or not. Must be either \code{TRUE} or \code{FALSE}. The default
#'  is \code{TRUE}.
#'  If set to \code{FALSE} the computation yields \code{NA} if \code{NA} values
#'  are present.
#' @return A positive numeric value, which indicates the sum of the top 5
#'  market shares as a percentage.
#' @examples
#' # a vector of market shares
#' x <- c(0.4, 0.2, 0.25, 0.1, 0.05)
#' concstats_top5(x)
concstats_top5 <- function(x, na.rm = TRUE) {
#' @srrstats {G5.8a} Zero-length data
#' @srrstats {G2.2, G2.6, G2.16} Checking class, type, NaN handling
  if (!is.numeric(x)) {
    stop("`x` in concstats_top5 must be a numeric vector\n",
         "You have provided an object of class:", class(x)[1])
  }
#' @srrstats {G2.0, G2.1}
  if (!is.logical(na.rm) || !length(na.rm) == 1 || is.na(na.rm)) {
    stop("`na.rm` in `concstats_top5` must be either TRUE or FALSE")
  }
#' @srrstats {G2.13, G2.14, G2.14a, G2.14b, G2.15} Handling of missing values
  if (na.rm == TRUE) {
    x <- as.numeric(x[!is.na(x)])
  }

  if (!na.rm && any(is.na(x))) return(NA_real_)

  # check if x is a positive decimal vector
  if (as.logical(all(x < 0))) {
    stop("x in `concstats_top5` must be a positive vector")
  }
#' @srrstats {G3.0, EA6.0, EA6.0e} Return values, single-valued objects.
  # check sum of vector. Must sum to 1
  if (!isTRUE(all.equal(sum(x), 1, tolerance = .Machine$double.eps^0.25))) {
    stop("vector `x` in `concstats_top5` does not sum to 1")
  }

#' @srrstats {G2.4, G2.4b} explicit conversion to continuous via `as.numeric()`
  if (sum(x, na.rm = TRUE) > 1) {
    x <-  as.numeric(x / sum(x, na.rm = TRUE))
  } else {
    x
  }

  x <- sort(x, decreasing = TRUE)
  top5 <- as.numeric(sum(x[1:5], na.rm = TRUE) * 100)
  return(top5)
}

#' @export
#' @title A wrapper for the proposed structural measures
#' @rdname concstats_all_mstruct
#' @param x A non-negative numeric vector.
#' @param na.rm A logical vector that indicates whether \code{NA} values should
#'  be excluded or not. Must be either \code{TRUE} or \code{FALSE}. The default
#'  is \code{TRUE}.
#'  If set to \code{FALSE} the computation yields \code{NA} if the vector
#'  contains \code{NA} values.
#' @param digits A non-null value for digits specifies the minimum number of
#'  significant digits to be printed in values. The default is \code{NULL} and
#'  will use base R print option. Significant digits defaults to 7.
#' @details
#'  \code{concstats_all_mstruct} returns all proposed group measures in a
#'   one step procedure with default settings if not otherwise specified.
#' @return A `data.frame`.
#' @srrstats {EA2.6}
#' @seealso [concstats_all_comp()], [concstats_all_inequ()]
#' @examples
#' # a vector of market shares
#' x <- c(0.4, 0.2, 0.25, 0.1, 0.05)
#' concstats_all_mstruct(x, digits = 2)
concstats_all_mstruct <- function(x, na.rm = TRUE, digits = NULL) {
#' @srrstats {G5.8a} Zero-length data
#' @srrstats {G2.2, G2.6, G2.16} Checking class, type, NaN handling
  if (!is.numeric(x)) {
    stop("`x` in concstats_all_mstruct must be a numeric vector\n",
         "You have provided an object of class:", class(x)[1])
  }
#' @srrstats {G2.0, G2.1}
  if (!is.logical(na.rm) || !length(na.rm) == 1 || is.na(na.rm)) {
    stop("`na.rm` in `concstats_all_mstruct` must be either TRUE or FALSE")
  }
#' @srrstats {G2.13, G2.14, G2.14a, G2.14b, G2.15} Handling of missing values
  if (na.rm == TRUE) {
    x <- as.numeric(x[!is.na(x)])
  }

  if (!na.rm && any(is.na(x))) return(NA_real_)

  # check if x is a positive decimal vector
  if (as.logical(all(x < 0))) {
    stop("x in `concstats_all_mstruct` must be a positive vector")
  }
#' @srrstats {G3.0, EA6.0, EA6.0e} Return values, single-valued objects.
  # check sum of vector. Must sum to 1
  if (!isTRUE(all.equal(sum(x), 1, tolerance = .Machine$double.eps^0.25))) {
    stop("vector `x` in `concstats_all_mstruct` does not sum to 1")
  }

#' @srrstats {G2.4, G2.4b} explicit conversion to continuous via `as.numeric()`
  if (sum(x, na.rm = TRUE) > 1) {
    x <-  as.numeric(x / sum(x, na.rm = TRUE))
  } else {
    x
  }

  invisible(utils::capture.output(
    firm <- concstats_firm(x, na.rm = TRUE),
    nrs_eq <- concstats_nrs_eq(x, na.rm = TRUE),
    top <- concstats_top(x, na.rm = TRUE),
    top3 <- concstats_top3(x, na.rm = TRUE),
    top5 <- concstats_top5(x, na.rm = TRUE)))
#' @srrstats {EA4.0, EA4.1, EA4.2, EA5.2, EA5.4} Numeric control of screen-based
#'  output.
  results_mstruct <- data.frame(Measure = c("Firms", "Nrs_equivalent",
                                            "Top (%)", "Top3 (%)", "Top5 (%)"),
                              Value = as.numeric(format(c(firm, nrs_eq, top,
                                                          top3, top5),
                                            scientific = FALSE,
                                            digits = digits,
                                            justify = "right")))

  return(results_mstruct)
}
