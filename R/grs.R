#' @title GRS measure
#'
#' @srrstats {G1.1} First implementation in R
#'
#' @usage
#'  concstats_grs(x, na.rm = TRUE, digits = NULL)
#'
#' @param x A non-negative numeric vector.
#' @param na.rm A logical vector that indicates whether \code{NA} values should
#'  be excluded or not. Must be either \code{TRUE} or \code{FALSE}. The default
#'  is \code{TRUE}. If set to \code{FALSE} the computation yields a message
#'  if the vector contains \code{NA} values. NAs will be removed for further
#'  computations.
#' @param digits An optional value for digits. Specifies the minimum number of
#'  significant digits to be printed in values. The default is \code{NULL} and
#'  will use base R print option.
#' @return A single numeric value.
#' @references Ginevicius, R. and S. Cirba (2009). "Additive measurement of
#'  market concentration", \emph{Journal of Business Economics and Management},
#'  10(3), 191-198.
#' @srrstats {G1.0} Primary reference
#' @srrstats {G5.4, G5.4a, G5.4b, G5.4c} First example data from reference,
#'  p. 196-197
#' @family Concentration and inequality measures
#' @rdname concstats_grs
#' @examples
#'
#' # a vector of market shares
#' x <- c(0.4, 0.3, 0.2, 0.1)
#' concstats_grs(x, digits = 3)
#' #[1] 0.398
#'
#' # a vector with NA values
#' x <- c(0.4, 0.2, 0.25, 0.1, 0.05, NA)
#' concstats_grs(x, na.rm = FALSE)
#'
#' @export
concstats_grs <- function(x, na.rm = TRUE, digits = NULL) {
#' @srrstats {G5.8a} Zero-length data
#' @srrstats {G2.2, G2.6, G2.16} Checking class, type, NaN handling

  checkmate::assert_int(x = digits, lower = 1, null.ok = TRUE)
  checkmate::qassert(x, "n[0,)")

#' @srrstats {G2.0, G2.14a, G2.14b} Implement assertions on lengths of inputs
#' @srrstats {G2.13, G2.14, G2.14a, G2.14b, G2.15} Handling of missing values
  if (!is.logical(na.rm) || !length(na.rm) == 1 || is.na(na.rm)) {
    stop("`na.rm` in `concstats_grs` must be either TRUE or FALSE")
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
    stop(" Your input vector `x` in `concstats_grs` does not sum to 1")
  }

  x <- as.numeric(x / sum(x, na.rm = TRUE))
  x <- sort(x, decreasing = TRUE)
  grs <- as.numeric(sum((length(x) ^ 2 * x[1] + 0.3 * x ^ 2) /
                          (length(x) ^ 2 + length(x) * 0.3 * x[1] * x) * x))
  if (!is.null(digits)) grs <- as.numeric(round(grs, digits = digits))

#' @srrstats {EA4.0, EA4.2, EA5.4}
  return(as.numeric(grs))
}
