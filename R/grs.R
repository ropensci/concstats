#' @title GRS measure
#'
#' @param x A non-negative numeric vector.
#' @param na.rm a logical vector that indicates whether \code{NA} values should
#'  be excluded or not. Must be either \code{TRUE} or \code{FALSE}. The default
#'  is \code{TRUE}.
#'  If set to \code{FALSE} the computation yields \code{NA} if vector contains
#'  \code{NA} values.
#' @return A single numeric measure in decimal form.
#' @references Ginevicius, R. and S. Cirba (2009). "Additive measurement of
#'  market concentration", \emph{Journal of Business Economics and Management},
#'  10(3), 191-198.
#'
#' @family Concentration and inequality measures
#' @rdname concstats_grs
#' @examples
#' # a vector of market shares
#' x <- c(0.4, 0.2, 0.25, 0.1, 0.05)
#' concstats_grs(x)
#' # a vector with NA values
#' x <- c(0.4, 0.2, 0.25, 0.1, 0.05, NA)
#' concstats_grs(x, na.rm = FALSE)
#'
#' @export
concstats_grs <- function(x, na.rm = TRUE) {
  if (!is.numeric(x)) {
    stop("`x` in concstats_grs must be a numeric vector\n",
         "You have provided an object of class:", class(x)[1])
  }

  if (!is.logical(na.rm) || !length(na.rm) == 1 || is.na(na.rm)) {
    stop("`na.rm` in `concstats_grs` must be either TRUE or FALSE")
  }

  if (na.rm == TRUE) {
    x <- as.numeric(x[!is.na(x)])
  }

  if (!na.rm && any(is.na(x))) return(NA_real_)

  # check if x is a positive decimal vector
  if (as.logical(all(x < 0))) {
    stop("x in `concstats_grs` must be a positive vector")
  }

  # check sum of vector. Must sum to 1 if all x(market share) < 1
  if (as.logical(all(x < 1) &&
                 !isTRUE(all.equal(sum(x), 1,
                                   tolerance = .Machine$double.eps^0.25)))) {
    stop("vector `x` in `concstats_grs` does not sum to 1")
  }

  # explicit conversion to continuous
  if (sum(x, na.rm = TRUE) > 1) {
    x <-  as.numeric(x / sum(x, na.rm = TRUE))
  } else {
    x
  }

  x <- sort(x, decreasing = TRUE)
  grs <- as.numeric(sum((length(x) ^ 2 * x[1] + 0.3 * x ^ 2) /
                          (length(x) ^ 2 + length(x) * 0.3 * x[1] * x) * x))
  return(grs)
}
