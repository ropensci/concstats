#' @title Herfindahl-Hirschman Index
#'
#' @srrstats {G1.4} roxygen2 used to document functions
#' @description
#' A measure of industry concentration and widely used in merger control.
#' @usage
#'  concstats_hhi(x, normalized = FALSE, na.rm = TRUE, digits = NULL)
#'
#' @param x A non-negative numeric vector.
#' @param normalized Logical. Argument specifying whether or not a normalized
#'  value is required. Must be either \code{TRUE} or \code{FALSE}. The default
#'   is \code{FALSE}.
#' @param na.rm A logical vector that indicates whether \code{NA} values should
#'  be excluded or not. Must be either \code{TRUE} or \code{FALSE}. The default
#'  is \code{TRUE}. If set to \code{FALSE} the computation yields a message
#'  if the vector contains \code{NA} values. NAs will be removed for further
#'  computations.
#' @srrstats {EA4.1}  control of numeric precision
#' @param digits A non-null value for digits specifies the minimum number of
#'  significant digits to be printed in values. The default is \code{NULL} and
#'  will use base R print option.
#' @details \code{concstats_hhi} calculates the widely used Herfindahl-Hirschman
#'  Index (Herfindahl, 1950 and Hirschman, 1945). The index is calculated by
#'  squaring the market share of each firm competing in the market and then
#'  summing the resulting numbers.
#' @return A single numeric measure in decimal form.
#' @references Herfindahl, O. C. (1950), "Concentration in the steel industry"
#'  (PhD thesis), Columbia University.
#' @references Hirschman, A. O. (1945), "National power and structure of
#'  foreign trade". Berkeley, CA: University of California Press.
#' @srrstats {G1.0} Primary reference
#'
#' @family Competition/Concentration measures
#' @rdname concstats_hhi
#' @examples
#' # a vector of market shares
#' x <- c(0.35, 0.4, 0.05, 0.1, 0.06, 0.04)
#' concstats_hhi(x, digits = 2)
#'
#' # a vector with NA values
#' x <- c(0.4, 0.2, 0.25, 0.1, 0.05, NA)
#' concstats_hhi(x, na.rm = TRUE)
#'
#' @export
concstats_hhi <- function(x, normalized = FALSE, na.rm = TRUE, digits = NULL) {
#' @srrstats {G5.8a} Zero-length data
#' @srrstats {G2.2, G2.6, G2.16} Checking class, type, NaN handling

  checkmate::assert_int(x = digits, lower = 1, upper = 7, null.ok = TRUE)
  checkmate::qassert(x, "n[0,)")
#' @srrstats {G2.0, G2.14a, G2.14b} Implement assertions on lengths of inputs
#' @srrstats {G2.13, G2.14, G2.14a, G2.14b, G2.15} Handling of missing values
  if (!is.logical(na.rm) || !length(na.rm) == 1 || is.na(na.rm)) {
    stop("`na.rm` in `concstats_hhi` must be either TRUE or FALSE")
  }

  if (!is.logical(normalized) || !length(normalized) == 1 || is.na(normalized)) {
    stop("`normalized` in `concstats_hhi` must be either TRUE or FALSE")
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
    stop(" Your input vector `x` in `concstats_hhi` does not sum to 1")
  }

  x <- as.numeric(x / sum(x, na.rm = TRUE))
  hhi <- as.numeric(sum(x ^ 2, na.rm = TRUE))
  if (normalized == TRUE)
    hhi <- as.numeric((hhi - (1 / sum(x > 0, na.rm = TRUE))) /
                        (1 - (1 / sum(x > 0, na.rm = TRUE))))
  if (!is.null(digits)) hhi <- as.numeric(round(hhi, digits = digits))
  return(as.numeric(hhi))
}
