#' @title Top market share
#'
#' @param x A non-negative numeric vector.
#' @param na.rm A logical vector that indicates whether \code{NA} values should
#'  be excluded or not. Must be either \code{TRUE} or \code{FALSE}. The default
#'  is \code{TRUE}.
#'  If set to \code{FALSE} the computation yields \code{NA} if \code{NA} values
#'  are present.
#' @return A positive numeric value, which indicates the top market share in
#'  percent.
#'
#' @family Market structure measures
#' @rdname concstats_top
#' @examples
#' # a vector of market shares
#' x <- c(0.4, 0.2, 0.25, 0.1, 0.05)
#' concstats_top(x)
#' @export
concstats_top <- function(x, na.rm = TRUE) {
  if (!is.numeric(x)) {
    stop("`x` in concstats_top must be a numeric vector\n",
         "You have provided an object of class:", class(x)[1])
  }

  if (!is.logical(na.rm) || !length(na.rm) == 1 || is.na(na.rm)) {
    stop("`na.rm` in `concstats_top` must be either TRUE or FALSE")
  }

  if (na.rm == TRUE) {
    x <- as.numeric(x[!is.na(x)])
  }

  if (!na.rm && any(is.na(x))) return(NA_real_)

  #check if x is a positive vector
  if (as.logical(any(x < 0))) {
    stop("`x` in `concstats_top` must be a positive vector")
  }

  # explicit conversion to continuous via `as.numeric()`
  if (sum(x, na.rm = TRUE) == 1 || sum(x, na.rm = TRUE) == 100) {
    x <-  as.numeric(x / sum(x, na.rm = TRUE))
  }

  # check sum of vector. Must sum to 1 if all x(market share) < 1
  if (as.logical(all(x < 1) &&
                 !isTRUE(all.equal(sum(x), 1,
                                   tolerance = .Machine$double.eps^0.25)))) {
    stop("vector `x` in `concstats_top` does not sum to 1")
  }

  x <- sort(x, decreasing = TRUE)
  top <- as.numeric(x[1], na.rm = TRUE) * 100
  return(top)
}
