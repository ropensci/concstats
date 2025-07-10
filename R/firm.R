#' @title Number of firms
#'
#' @srrstats {G1.4} roxygen2 used to document functions
#' @usage
#'  concstats_firm(x, na.rm = TRUE)
#' @param x A non-negative numeric vector.
#' @param na.rm A logical vector that indicates whether \code{NA} values should
#'  be excluded or not. Must be either \code{TRUE} or \code{FALSE}. The default
#'  is \code{TRUE}. If set to \code{FALSE} the computation yields a message
#'  if the vector contains \code{NA} values. NAs will be removed for further
#'  computations.
#' @return A positive integer.
#'
#' @family Market structure measures
#' @rdname concstats_firm
#' @examples
#' # a vector of market shares
#' x <- c(0.4, 0.2, 0.25, 0.1, 0.05)
#' concstats_firm(x)
#'
#' @export
concstats_firm <- function(x, na.rm = TRUE) {
  checkmate::qassert(x, "n[0,)")

#' @srrstats {G2.0, G2.14a, G2.14b} Implement assertions on lengths of inputs
#' @srrstats {G2.13, G2.14, G2.14a, G2.14b, G2.15} Handling of missing values
  if (!is.logical(na.rm) || !length(na.rm) == 1 || is.na(na.rm)) {
    stop("`na.rm` in `concstats_firm` must be either TRUE or FALSE")
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
    stop(" Your input vector `x` in `concstats_firm` does not sum to 1")
  }

  x <- as.numeric(x / sum(x, na.rm = TRUE))
  firm <- as.numeric(sum(x > 0), na.rm = TRUE)
#' @srrstats {G2.4a} explicit conversion to integer via `as.integer`
  return(as.integer(firm))
}
