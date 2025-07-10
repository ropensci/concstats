#' @title Gini coefficient
#'
#' @srrstats {G1.4} roxygen2 used to document functions
#' @usage
#'  concstats_gini(x, normalized = TRUE, na.rm = TRUE, digits = NULL)
#'
#' @param x A non-negative numeric vector.
#' @param normalized Logical. Argument specifying whether or not a normalized
#'  value is required. Must be either \code{TRUE} or \code{FALSE}. The default
#'  is \code{TRUE}.
#' @param na.rm A logical vector that indicates whether \code{NA} values should
#'  be excluded or not. Must be either \code{TRUE} or \code{FALSE}. The default
#'  is \code{TRUE}. If set to \code{FALSE} the computation yields \code{NA}
#'  if \code{NA} values are present.
#' @srrstats {EA4.1}  control of numeric precision
#' @param digits An optional value for digits. Specifies the minimum number of
#'  significant digits to be printed in values. The default is \code{NULL} and
#'  will use base R print option. Significant digits defaults to 7. Values are
#'  restricted between 1 and default value.
#' @return A single numeric value.
#' @family Concentration and inequality measures
#' @rdname concstats_gini
#' @examples
#' # a vector of market shares
#' x <- c(0.4, 0.2, 0.25, 0.1, 0.05)
#' concstats_gini(x, normalized = TRUE)
#'
#' # a vector with NA values
#' x <- c(0.4, 0.2, 0.25, 0.1, 0.05, NA)
#' concstats_gini(x, na.rm = TRUE, digits = 2)
#'
#' @export
concstats_gini <- function(x, normalized = TRUE, na.rm = TRUE, digits = NULL) {
#' @srrstats {G5.8a} Zero-length data
#' @srrstats {G2.2, G2.6, G2.16} Checking class, type, NaN handling
  checkmate::check_logical(x = normalized, all.missing = FALSE, min.len = 1,
                           null.ok = FALSE)
  checkmate::assert_int(x = digits, lower = 1, null.ok = TRUE)
  checkmate::qassert(x, "n[0,)")

#' @srrstats {G2.13, G2.14, G2.14a, G2.14b, G2.15} Handling of missing values
  if (!is.logical(na.rm) || !length(na.rm) == 1 || is.na(na.rm)) {
    stop("`na.rm` in `concstats_gini` must be either TRUE or FALSE")
  }

  if (!is.logical(normalized) || !length(normalized) == 1 || is.na(normalized)) {
    stop("`normalized` in `concstats_gini` must be either TRUE or FALSE")
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
    stop("Your input vector `x` in `concstats_gini` does not sum to 1")
  }

  x <- as.numeric(x / sum(x, na.rm = TRUE))
  x <- sort(x)
  gini <- as.numeric(2 * sum(x * seq_len(length(x))) /
                       (length(x) * sum(x)) - 1 - (1 / length(x)))
  if (normalized) gini <- as.numeric(length(x) / (length(x) - 1) * gini)
  if (!is.null(digits)) gini <- as.numeric(round(gini, digits = digits))
  return(as.numeric(gini))
}
