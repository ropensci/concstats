#' @title shares
#' @srrstats {G1.4} roxygen2 used to document functions
#' @description
#' The \code{concstats_shares} function is a helper function making it easier
#' to convert numeric variable into individual shares. This might be convenient
#' for larger vectors.
#'
#' @usage
#'  concstats_shares(x, na.rm = TRUE, digits = NULL)
#' @param x A non-negative numeric vector.
#' @param na.rm A logical vector that indicates whether \code{NA} values should
#'  be excluded or not. Must be either \code{TRUE} or \code{FALSE}. The default
#'  is \code{TRUE}. If set to \code{FALSE} the computation yields a message
#'  if the vector contains \code{NA} values. NAs will be removed for further
#'  computations.
#' @srrstats {EA4.1}  control of numeric precision
#' @param digits An optional value for digits. Specifies the minimum number of
#'  significant digits to be printed in values. The default is \code{NULL} and
#'  will use base R print option.
#' @details \code{concstats_shares} is a helper function. The user can manually
#'  convert or provide numerical vectors of shares or use
#'  \code{constats_shares}.
#' @return A numeric vector in decimal form.
#'
#' @rdname concstats_shares
#'
#' @examples
#' # a vector of loans (without special characters, e.g. currency symbols)
#' x <- c(538572286.08, 481096.77, 161914143.03, 128796268.59, 69055940.72)
#' concstats_shares(x)
#'
#' # a vector with NA values
#' x2 <- c(538572286.08, 481096.77, 161914143.03, 128796268.59, 69055940.72, NA)
#' concstats_shares(x2, na.rm = TRUE, digits = 3)
#'
#' @export
concstats_shares <- function(x, na.rm = TRUE, digits = NULL) {
#' @srrstats {G2.1} Assertions on types of inputs
#' @srrstats {G5.8a} Zero-length data
#' @srrstats {G2.2, G2.6, G2.16} Checking class, type, NaN handling

  checkmate::assert_int(x = digits, lower = 1, null.ok = TRUE)
  checkmate::qassert(x, "n[0,)")
#' @srrstats {G2.0, G2.14a, G2.14b} Implement assertions on lengths of inputs
#' @srrstats {G2.13, G2.14, G2.14a, G2.14b, G2.15} Handling of missing values
  if (!is.logical(na.rm) || !length(na.rm) == 1 || is.na(na.rm)) {
    stop("`na.rm` in `concstats_shares` must be either TRUE or FALSE")
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
    stop(" Your input vector `x` in `concstats_shares` does not sum to 1")
  }

  x <- as.numeric(x / sum(x, na.rm = TRUE))

  if (!is.null(digits)) x <- as.numeric(round(x, digits = digits))
  return(as.numeric(x))

}
