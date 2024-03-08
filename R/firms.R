#' @title Number of firms
#'
#' @param x A non-negative numeric vector.
#' @param na.rm Logical vector that indicates whether \code{NA} values should
#'  be excluded or not. Must be either \code{TRUE} or \code{FALSE}. The default
#'  is \code{TRUE}.
#'  If set to \code{FALSE} the computation yields \code{NA} if the vector
#'  contains \code{NA} values.
#' @return A positive integer.
#'
#' @family Market structure measures
#' @rdname concstats_firm
#' @examples
#' # a vector of market shares
#' x <- c(0.4, 0.2, 0.25, 0.1, 0.05)
#' concstats_firm(x)
#'
#'
#' @export
concstats_firm <- function(x, na.rm = TRUE) {
  if (!is.numeric(x)) {
    stop("`x` in concstats_firm must be a numeric vector\n",
         "You have provided an object of class:", class(x)[1])
  }

  if (!is.logical(na.rm) || !length(na.rm) == 1 || is.na(na.rm)) {
    stop("`na.rm` in `concstats_firm` must be either TRUE or FALSE")
  }

  if (na.rm == TRUE) {
    x <- as.numeric(x[!is.na(x)])
  }

  if (!na.rm && any(is.na(x))) return(NA_real_)

  # check if x is a positive decimal vector
  if (as.logical(any(x < 0))) {
    stop("x in `concstats_firm` must be a positive vector")
  }

  # explicit conversion to continuous via `as.numeric()`
  shares <- try(sum(x, na.rm = TRUE))
  if (shares == 100 || shares == 1) {
    x <-  as.numeric(x / sum(x, na.rm = TRUE))
  }

  # check sum of vector. Must sum to 1 if all x(market share) < 1
  if (as.logical(all(x < 1) &&
                 !isTRUE(all.equal(sum(x), 1,
                                   tolerance = .Machine$double.eps^0.25)))) {
    stop("vector `x` in `concstats_firm` does not sum to 1")
  }


  firm <- as.numeric(sum(x > 0), na.rm = TRUE)
  return(firm)
}
