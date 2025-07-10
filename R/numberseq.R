#' @title Numbers equivalent
#' @srrstats {G1.4} roxygen2 used to document functions
#' @usage
#'  concstats_nrs_eq(x, na.rm = TRUE, digits = NULL)
#' @param x A non-negative numeric vector.
#' @param na.rm A logical vector that indicates whether \code{NA} values should
#'  be excluded or not. Must be either \code{TRUE} or \code{FALSE}. The default
#'  is \code{TRUE}.
#'  If set to \code{FALSE} the computation yields \code{NA} if \code{NA} values
#'  are present.
#' @srrstats {EA4.1}  control of numeric precision
#' @param digits An optional value for digits. Specifies the minimum number of
#'  significant digits to be printed in values. The default is \code{NULL} and
#'  will use base R print option.
#' @srrstats {G2.0a, G2.1a, EA1.1, EA1.3} accepted as input, length and type
#' @return A positive numeric value, rounded to the second decimal place.
#'
#' @family Market structure measures
#' @rdname concstats_nrs_eq
#' @examples
#' # a vector of market shares
#' x <- c(0.4, 0.2, 0.25, 0.1, 0.05)
#' concstats_nrs_eq(x, digits = 2)
#'
#' @export
concstats_nrs_eq <- function(x, na.rm = TRUE, digits = NULL) {
#' @srrstats {G2.1} Assertions on types of inputs
#' @srrstats {G5.8a} Zero-length data
#' @srrstats {G2.2, G2.6, G2.16} Checking class, type, NaN handling

  checkmate::assert_int(x = digits, lower = 1, null.ok = TRUE)
  checkmate::qassert(x, "n[0,)")
#' @srrstats {G2.13, G2.14, G2.14a, G2.14b, G2.15} Handling of missing values
  if (!is.logical(na.rm) || !length(na.rm) == 1 || is.na(na.rm)) {
    stop("`na.rm` in `concstats_nrs_eq` must be either TRUE or FALSE")
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
    stop(" Your input vector `x` in `concstats_nrs_eq` does not sum to 1")
  }

  x <- as.numeric(x / sum(x, na.rm = TRUE))
  nrs_eq <- as.numeric(1 / sum(x ^ 2))
  if (!is.null(digits)) nrs_eq <- as.numeric(round(nrs_eq, digits = digits))
  return(as.numeric(nrs_eq))
}
