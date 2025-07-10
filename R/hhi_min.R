#' @title Minimum of Herfindahl-Hirschman Index
#'
#' @srrstats {G1.4} roxygen2 used to document functions
#' @usage
#'  concstats_hhi_min(x, na.rm = TRUE, digits = NULL)
#' @param x A non-negative numeric vector.
#' @param na.rm A logical vector that indicates whether \code{NA} values
#'  should be excluded or not. Must be either \code{TRUE} or \code{FALSE}.
#'  The default is \code{TRUE}. If set to \code{FALSE} the computation
#'  yields a message if the vector contains \code{NA} values. NAs will
#'  be removed for further computations.
#' @srrstats {EA4.1}  control of numeric precision
#' @param digits A non-null value for digits specifies the minimum number of
#'  significant digits to be printed in values. The default is \code{NULL} and
#'  will use base R print option.
#' @details Calculates the minimum of the Herfindahl-Hirschman index, that is,
#'  the equivalent of all participants in the market with equal market shares.
#' @return A single numeric measure in decimal form.
#' @family Competition/Concentration measures
#' @rdname concstats_hhi_min
#' @examples
#' # a vector of market shares
#' x <- c(0.35, 0.4, 0.05, 0.1, 0.06, 0.04)
#' concstats_hhi_min(x, digits = 2)
#'
#' # a vector with NA values
#' x <- c(0.4, 0.2, 0.25, 0.1, 0.05, NA)
#' concstats_hhi_min(x, na.rm = FALSE)
#'
#' @export
concstats_hhi_min <- function(x, na.rm = TRUE, digits = NULL) {
#' @srrstats {G5.8a} Zero-length data
#' @srrstats {G2.2, G2.6, G2.16} Checking class, type, NaN handling

  checkmate::assert_int(x = digits, lower = 1, null.ok = TRUE)
  checkmate::qassert(x, "n[0,)")
#' @srrstats {G2.0, G2.14a, G2.14b} Implement assertions on lengths of inputs
#' @srrstats {G2.13, G2.14, G2.14a, G2.14b, G2.15} Handling of missing values
  if (!is.logical(na.rm) || !length(na.rm) == 1 || is.na(na.rm)) {
    stop("`na.rm` in `concstats_hhi_min` must be either TRUE or FALSE")
  }

  if (na.rm == TRUE && anyNA(x)) {
    message("`x` has NA values. NAs have been removed for computation.")
    x <- x[!is.na(x)]
  }

  if (!na.rm && anyNA(x)) return(NA_real_)

  x <- as.numeric(x / sum(x, na.rm = TRUE))

  # check sum of vector. Must sum to 1 if all x(market share) < 1
  if (!isTRUE(all.equal(sum(x, na.rm = TRUE),
                        1, tolerance = .Machine$double.eps^0.25))) {
    stop(" Your input vector `x` in `concstats_hhi_min` does not sum to 1")
  }

  x <- as.numeric(x / sum(x, na.rm = TRUE))

  x <- as.numeric(x[!is.na(x)] / sum(x[!is.na(x)]))
  hhi_min <- as.numeric(1 / sum(x > 0))
  if (!is.null(digits)) hhi_min <- as.numeric(round(hhi_min, digits = digits))
  return(as.numeric(hhi_min))
}
