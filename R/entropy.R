#' @title Shannon Entropy
#'
#' @srrstats {G1.4} roxygen2 used to document functions
#' @usage
#'  concstats_entropy(x, normalized = TRUE, na.rm = TRUE, digits = NULL)
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
#'  will use base R print option.
#' @return A single numeric measure.
#' @references Shannon, C. E. (1948). "A Mathematical Theory of Communication",
#'  \emph{The Bell System Technical Journal} (Nokia Bell Labs).
#' @srrstats {G1.0} Primary reference
#' @family Concentration and inequality measures
#' @rdname concstats_entropy
#' @examples
#' # a vector of market shares
#' x <- c(0.4, 0.2, 0.25, 0.1, 0.05)
#' concstats_entropy(x, normalized = TRUE)
#'
#' # a vector with NA values
#' x <- c(0.4, 0.2, 0.25, 0.1, 0.05, NA)
#' concstats_entropy(x, na.rm = TRUE, digits = 2)
#'
#' @export
concstats_entropy <- function(x, normalized = TRUE, na.rm = TRUE,
                              digits = NULL) {

#' @srrstats {G5.8a} Zero-length data
#' @srrstats {G2.2, G2.6, G2.16} Checking class, type, NaN handling

  checkmate::assert_int(x = digits, lower = 1, null.ok = TRUE)
  checkmate::qassert(x, "n[0,)")

#' @srrstats {G2.13, G2.14, G2.14a, G2.14b, G2.15} Handling of missing values
  if (!is.logical(na.rm) || !length(na.rm) == 1 || is.na(na.rm)) {
    stop("`na.rm` in `concstats_entropy` must be either TRUE or FALSE")
  }

  if (!is.logical(normalized) || !length(normalized) == 1 || is.na(normalized)) {
    stop("`normalized` in `concstats_entropy` must be either TRUE or FALSE")
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
    stop(" Your input vector `x` in `concstats_entropy` does not sum to 1")
  }

  x <- as.numeric(x / sum(x, na.rm = TRUE))
  entropy <- as.numeric((sum(-x / sum(x) * log(x / sum(x), base = 2))
                         / log(sum(x > 0), base = 2)))
  if (normalized == FALSE) entropy <- as.numeric(
    sum(-x / sum(x) * log(x / sum(x), base = 2))
  )
  if (!is.null(digits)) entropy <- as.numeric(round(entropy, digits = digits))
  return(as.numeric(entropy))
}
